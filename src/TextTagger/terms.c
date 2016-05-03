/* terms.c - Find terms in lines of standard input from a large terms list file.
 * William de Beaumont
 * 2009-07-01
 *
 * This needed to be in C because the Perl version used way too much memory.
 * The C version simply reads in the entire terms file, builds a term index,
 * and does a binary search on that index for each substring on stdin that
 * could be a term. This means that the terms file needs to be sorted (as if by
 * wcscmp, which is to say (roughly speaking), alphabetically). I used wchar_t
 * instead of char because the geographic names database contains some Unicode
 * characters. This makes the term list in memory about 4 times larger than the
 * file (sizeof(wchar_t)=4), but that's still much less than the Perl version.

Example:

% gcc terms.c -o terms
% ./terms terms.txt 2>/dev/null
Garden Street Mall			<-- This is an example input.
Garden	0	6			<-- Each term is printed on its own
Garden Street Mall	0	18	    line, along with start and end
Street	7	13			    character indices.
Mall	14	18
					<-- A blank line indicates the program
					    is done with this line of input.
^D					<-- EOF on stdin causes the program to
					    end.
%

 * The terms file has one term per line, optionally followed by a tab and some
 * extra fields to be reported when that term is seen. These extra fields will
 * be added to the output line after the start and end character indices, for
 * example, if this was a line in terms.txt:

carrying out	V	ING	CARRY

 * and the input "carrying out" were given, the corresponding output line would
 * be:

carrying out	0	12	V	ING	CARRY

 */

#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <locale.h>
#include <wchar.h>
#include <wctype.h>
#include <errno.h>

#define MAX_INPUT_LENGTH 8192

/* We need to dereference an extra time in order for bsearch to work. */
int wcscmp2(const void* v1, const void* v2) {
  const wchar_t** s1 = (const wchar_t**)v1;
  const wchar_t** s2 = (const wchar_t**)v2;
  return wcscmp(*s1, *s2);
}

/* Is term completely composed of capital letters and spaces? */
bool all_caps_p(const wchar_t* term, int length) {
  int i;
  for (i = 0; i < length; i++)
    if ((!iswupper(term[i])) && (!iswspace(term[i])))
      return false;
  return true;
}

/* Modify term so that only the first letter of each word is capitalized.
 * Assumes all_caps_p returned true. */
void to_initial_caps(wchar_t* term, int length) {
  int i;
  bool first_letter = true;
  for (i = 0; i < length; i++) {
    if (!first_letter)
      term[i] = towlower(term[i]);
    first_letter = iswspace(term[i]);
  }
}

/* Modify term so that no letters are capitalized. */
void to_no_caps(wchar_t* term, int length) {
  int i;
  for (i = 0; i < length; i++) {
    term[i] = towlower(term[i]);
  }
}

/* The entire contents of the terms file, with newlines and the first tab on
 * each line replaced by nulls. */
wchar_t* terms_text = NULL;
/* An array of pointers into terms_text at the beginnings of lines. */
wchar_t** terms = NULL;

size_t num_chars = 0, /* The number of characters read from the terms file. */
       num_terms = 0, /* The number of lines read from the terms file. */
       max_term_length = 0; /* The maximum number of characters in any term
			     * read from the terms file. */

void print_found_term(wchar_t** found, size_t start, size_t length) {
  /* Find the end of the line for this term in terms_text. */
  wchar_t* end_of_line = ((found == &(terms[num_terms - 1])) ? /* last term? */
                          &(terms_text[num_chars - 1]) : /* end of last line */
			  &(found[1][-1])); /* beginning of next term - 1 */
  wprintf(L"%ls\t%d\t%d", *found, start, start + length);
  if (&((*found)[length]) < end_of_line) { /* There was more on this line. */
    wprintf(L"\t%ls", &((*found)[length+1])); /* Print it! */
  }
  wprintf(L"\n");
}

/* Handle an error from a file input function, and return what to do in the
 * enclosing loop. */
typedef enum { CONTINUE, BREAK, EXIT } error_decision;
error_decision handle_input_error(FILE* f, char* msg) {
  int errno_backup = errno;
  /* error while reading */
  if (feof(f)) /* done reading */
    return BREAK;
  if (errno_backup != EILSEQ) {/* ILlegal byte SEQuence */
    /* all other errors, just print and quit */
    errno = errno_backup;
    perror(msg);
    return EXIT;
  }
  /* Clear the error and read one byte, trying to get past invalid
   * multibyte characters... */
  clearerr(f);
  char buf[1];
  fread(buf, 1, 1, f);
  return CONTINUE;
}

int main(int argc, char** argv) {
  /* Open (and get the size of) the terms file specified as the first argument,
   * handling errors. */
  if (argc != 2) {
    fprintf(stderr, "USAGE: %s /path/to/terms.txt\n", argv[0]);
    return 1;
  }
  if (!setlocale(LC_ALL, "en_US.UTF-8")) {
    fprintf(stderr, "Warning, failed to set locale\n");
  }
  struct stat s;
  if (stat(argv[1], &s) != 0) {
    perror("Can't stat terms file");
    return 2;
  }
  if (!S_ISREG(s.st_mode)) {
    fprintf(stderr, "terms file is not a regular file");
    return 3;
  }
  size_t num_bytes = s.st_size + 1; /* +1 to accomodate the null fgetws adds */
  FILE* f = fopen(argv[1], "r");
  if (f == NULL) {
    perror("Error opening terms file");
    return 4;
  }
  /* Allocate one big array for the entire file. This overestimates the number
   * of characters, since some characters are more than one byte long, but
   * that's OK. */
  terms_text = (wchar_t*)malloc(sizeof(wchar_t) * num_bytes);
  /* Read the whole file into terms_text, separating lines with null
   * characters, and filling in the global size_t s above. */
  for(;;) {
    if (num_chars >= num_bytes - 1) { /* we already got all the bytes */
      break;
    } else if (fgetws(&(terms_text[num_chars]), num_bytes - num_chars, f) == NULL) {
      error_decision d = handle_input_error(f, "Error while reading terms file");
      if (d == EXIT) {
	return 5;
      } else if (d == BREAK) {
	break;
      }
    } else { /* no error while reading */
      /* Find the length of this term, while incrementing num_chars. */
      int this_term_length = 0;
      for (;
           terms_text[num_chars] != L'\0' &&
	   terms_text[num_chars] != L'\t' &&
	   terms_text[num_chars] != L'\n';
	   num_chars++, this_term_length++)
	;
      /* Updated max_term_length if necessary. */
      if (max_term_length < this_term_length)
	max_term_length = this_term_length;
      /* If we stopped at a tab, eat up input until the next newline or null */
      for (;
           terms_text[num_chars] != L'\0' &&
	   terms_text[num_chars] != L'\n';
	   num_chars++)
	;
      terms_text[num_chars++] = L'\0'; /* replace newline with null */
      num_terms++;
    }
  }
  /* Make a smaller array containing pointers into terms_text at the beginnings
   * of lines, and convert the first tab on each line to a null character. */
  terms = (wchar_t**)malloc(sizeof(wchar_t*) * num_terms);
  terms[0] = terms_text;
  size_t term_index = 1;
  wchar_t* char_index;
  bool before_tab = true;
  for (char_index = terms_text;
       char_index < &(terms_text[num_chars]);
       char_index++) {
    if (*char_index == L'\0') {
      if (term_index < num_terms) {
	terms[term_index++] = char_index + 1;
	before_tab = true;
      } else {
	break;
      }
    } else if (before_tab && *char_index == L'\t') {
      *char_index = L'\0';
      before_tab = false;
    }
  }

  /* Read lines from stdin, and find all the terms in them, writing them (with
   * start and end character indices, and following fields) to stdout, followed
   * by a blank line to indicate the end of output for a given input line. */
  wchar_t input[MAX_INPUT_LENGTH];
  wchar_t* term = (wchar_t*)malloc(sizeof(wchar_t) * (max_term_length + 1));
  for(;;) {
    if (fgetws(input, MAX_INPUT_LENGTH, stdin) == NULL) {
      error_decision d = handle_input_error(stdin, "Error while reading stdin");
      if (d == EXIT) {
	return 6;
      } else if (d == BREAK) {
	break;
      }
    } else { /* no error while reading */
      size_t input_length = wcslen(input)-1,
             term_start,
	     term_length;
      /* For each substring of the input short enough to be a term and bounded
       * by word boundaries... */
      for (term_start = 0; term_start < input_length; term_start++) {
	if (term_start == 0 ||
	    ((!iswalnum(input[term_start-1])) &&
	     (!iswspace(input[term_start])))) {
	  /* The start of the term is at a word boundary. */
	  for (term_length = 1;
	       term_length <= max_term_length && 
	       term_start + term_length <= input_length;
	       term_length++) {
	    if (term_start + term_length == input_length ||
	        ((!iswalnum(input[term_start + term_length])) &&
		 (!iswspace(input[term_start + term_length - 1])))) {
	      /* The end of the term is at a word boundary. */
	      wchar_t** found = NULL; /* A pointer to the element of terms
				       * bsearch found. */
	      /* Copy the input substring into term, with a null termination. */
	      wcsncpy(term, &(input[term_start]), term_length);
	      term[term_length] = L'\0';
	      /* Search for the term in the list. */
	      if ((found =
		   (wchar_t**)bsearch(&term, terms, num_terms,
		                      sizeof(wchar_t*), &wcscmp2)
		  ) != NULL) {
		/* If found, print the term, and the start and end character
		 * indices, separated by spaces. */
		print_found_term(found, term_start, term_length);
	      } else if (all_caps_p(term, term_length)) {
		/* If we didn't find the term, but it was ALL CAPS, convert the
		 * term to Initial Caps and try again. */
		to_initial_caps(term, term_length);
		if ((found =
		     (wchar_t**)bsearch(&term, terms, num_terms,
					sizeof(wchar_t*), &wcscmp2)
		    ) != NULL) {
		  print_found_term(found, term_start, term_length);
		}
	      }
	      if (found == NULL) {
		/* If we still didn't find the term, try no caps. */
		to_no_caps(term, term_length);
		if ((found =
		     (wchar_t**)bsearch(&term, terms, num_terms,
					sizeof(wchar_t*), &wcscmp2)
		    ) != NULL) {
		  print_found_term(found, term_start, term_length);
		}
	      }
	    }
	  }
	}
      }
      /* Print a blank line to indicate that we're done processing this line of
       * input. */
      wprintf(L"\n");
      fflush(stdout);
    }
  }
  return 0;
}

