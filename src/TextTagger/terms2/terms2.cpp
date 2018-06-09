/* terms2 - Find terms in lines of standard input from a terms list file too
 * large to fit in RAM comfortably.
 * William de Beaumont
 * 2015-10-26
 *
 * This is like terms.c, but instead of storing the whole terms file in RAM, we
 * store only every nth term along with its byte offset in the file. We search
 * using the C++ std::lower_bound function, seek to the nearest byte offset,
 * and read terms until we find the one we want or know it's not there.
 */

#include <wchar.h>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#ifdef USE_C_STYLE_SETLOCALE
#include <clocale>
#else
#include <locale>
#endif
#include <stdexcept>
#include <vector>
#include <set>
#include <algorithm>
#include "bloom_filter.h"

#define ESTIMATED_NUM_TERMS 10000000
#define FALSE_POSITIVE_RATE 0.01
#define TERMS_PER_INDEX_ENTRY 100

struct IndexEntry {
  std::wfstream::pos_type file_pos;
  std::wstring term;

  IndexEntry(std::wfstream::pos_type file_pos, std::wstring term)
  : file_pos(file_pos), term(term)
  {}

  bool operator<(std::wstring search_term) const {
    return (term < search_term);
  }
};

// read a line from a vocabulary file, get the term, and discard the info
bool read_term_from_vocab_file(std::wistream& file, std::wstring& term) {
  std::wstring line;
  if (!std::getline(file, line))
    return false;
  if (line.empty())
    std::wcerr << "warning: read empty line at pos " << file.tellg() << std::endl;
  std::wstringstream line_stream(line);
  std::getline(line_stream, term, L'\t');
  return true;
}

// read a line from a vocabulary file, get the term and the info
bool read_line_from_vocab_file(std::wistream& file, std::wstring& term, std::wstring& info) {
  std::wstring line;
  if (!std::getline(file, line))
    return false;
  if (line.empty())
    std::wcerr << "warning: read empty line at pos " << file.tellg() << std::endl;
  std::wstringstream line_stream(line);
  std::getline(line_stream, term, L'\t');
  std::getline(line_stream, info);
  return true;
}

class Vocabulary {
  std::wfstream file;
  std::vector<IndexEntry> index;
  size_t max_term_length;
  size_t num_terms; // for debugging purposes
  BloomFilter filter;

  public:

    Vocabulary(const char* filename)
    : file(filename, std::wfstream::in),
      index(),
      max_term_length(0),
      num_terms(0),
      filter(ESTIMATED_NUM_TERMS, FALSE_POSITIVE_RATE)
    {
      // construct index and add terms to filter
      std::wstring term;
      for(;;) {
	// read a term and add an index entry
	term.clear();
	std::wfstream::pos_type file_pos = file.tellg();
	if (!read_term_from_vocab_file(file, term)) {
	  if (!file.eof())
	    std::wcerr << "error reading vocabulary file" << std::endl;
	  return;
	}
	num_terms++;
	// index.emplace_back(file_pos, term);
	IndexEntry e(file_pos, term);
	index.push_back(e);
	filter.add_key(term);
	if (term.length() > max_term_length)
	  max_term_length = term.length();
	// read a bunch more lines
	for (int i = 1; i < TERMS_PER_INDEX_ENTRY; ++i) {
	  term.clear();
	  if (!read_term_from_vocab_file(file, term)) {
	    if (!file.eof())
	      std::wcerr << "error reading vocabulary file" << std::endl;
	    return;
	  }
	  num_terms++;
	  filter.add_key(term);
	  if (term.length() > max_term_length)
	    max_term_length = term.length();
	}
      }
    }

    size_t get_max_term_length() const { return max_term_length; }
    size_t get_num_terms() const { return num_terms; }
    size_t get_index_size() const { return index.size(); }

    bool look_up(std::wstring search_term, std::wstring& info) {
      if (!filter.may_contain_key(search_term))
	return false;
      // look up the term in the index and seek to a spot in the file before
      // where the term would be
      std::vector<IndexEntry>::const_iterator i =
        std::lower_bound(index.begin(), index.end(), search_term);
      if (i == index.end() || search_term != i->term) {
	if (i == index.begin())
	  return false;
	i--;
      }
      file.clear();
      file.seekg(i->file_pos);
      // read lines in the file until we find the term or something not less
      // than it
      std::wstring term;
      for(;;) {
	term.clear();
	info.clear();
	read_line_from_vocab_file(file, term, info);
	if (search_term == term) { // found it!
	  return true;
	} else if (file.eof() || search_term < term) { // it's not here!
	  return false;
	}
	// haven't found it yet, keep searching
      }
    }
};

void print_found_term(const std::wstring& term, size_t term_start, size_t term_length, const std::wstring& info) {
  std::wcout << term << L'\t'
	     << term_start << L'\t'
	     << (term_start + term_length) << L'\t'
	     << info << std::endl;
}

// Would the character c match perl's \s regexp?
// Note: wctype's iswspace is missing many of these space characters
bool is_perl_space(wchar_t c) {
  // this list from perldoc perlrecharclass 
  return (
    c == 0x0020 || // SPACE
    (c >= 0x0009 && c <= 0x000d) || // ASCII control chars
    (c >= 0x2000 && c <= 0x200a) || // Unicode spaces of various widths
    c == 0x0085 || // NEL
    c == 0x00a0 || // NO-BREAK SPACE
    c == 0x1680 || // OGHAM SPACE MARK
    c == 0x2028 || // LINE SEPARATOR
    c == 0x2029 || // PARAGRAPH SEPARATOR
    c == 0x202f || // NARROW NO-BREAK SPACE
    c == 0x205f || // MEDIUM MATHEMATICAL SPACE
    c == 0x3000    // IDEOGRAPHIC SPACE
  );
}

// Is term completely composed of capital letters and spaces?
bool all_caps_p(const std::wstring& term) {
  for (std::wstring::const_iterator i = term.begin(); i != term.end(); ++i)
    if ((!iswupper(*i)) && (!is_perl_space(*i)))
      return false;
  return true;
}

// Modify term so that only the first letter of each word is capitalized.
// Assumes all_caps_p returned true.
void to_initial_caps(std::wstring& term) {
  bool first_letter = true;
  for (std::wstring::iterator i = term.begin(); i != term.end(); ++i) {
    if (!first_letter)
      *i = towlower(*i);
    first_letter = is_perl_space(*i);
  }
}

// Does term have any capital letters?
bool any_caps_p(const std::wstring& term) {
  // return std::any_of(term.begin(), term.end(), &iswupper);
  for (std::wstring::const_iterator i = term.begin(); i != term.end(); ++i)
    if (iswupper(*i))
      return true;
  return false;
}

// Does term have any lower-case letters?
bool any_lower_p(const std::wstring& term) {
  // return std::any_of(term.begin(), term.end(), &iswlower);
  for (std::wstring::const_iterator i = term.begin(); i != term.end(); ++i)
    if (iswlower(*i))
      return true;
  return false;
}

// Modify term so that no letters are capitalized.
void to_no_caps(std::wstring& term) {
  std::transform(term.begin(), term.end(), term.begin(), &towlower);
}

// Modify term so that all letters are capitalized.
void to_all_caps(std::wstring& term) {
  std::transform(term.begin(), term.end(), term.begin(), &towupper);
}

wchar_t to_normal_space_char(wchar_t c) {
  return (is_perl_space(c) ? L' ' : c);
}

// Modify term so that all whitespace characters are the normal ASCII space
void to_normal_space(std::wstring& term) {
  std::transform(term.begin(), term.end(), term.begin(), &to_normal_space_char);
}

// Is c some kind of dash?
bool iswdash(wchar_t c) {
  return (c == L'-' || // ASCII hyphen-minus
	  c == L'‒' || // figure dash
	  c == L'–' || // en dash
	  c == L'—' || // em dash
	  c == L'―' || // horizontal bar
	  c == L'−'); // minus
}

// Does term have any dashes (ASCII or Unicode)?
bool any_dashes_p(const std::wstring& term) {
  // return std::any_of(term.begin(), term.end(), &iswdash);
  for (std::wstring::const_iterator i = term.begin(); i != term.end(); ++i)
    if (iswdash(*i))
      return true;
  return false;
}

// Modify term so all dashes are deleted.
void to_no_dashes(std::wstring& term) {
  term.erase(std::remove_if(term.begin(), term.end(), &iswdash), term.end());
}

struct WordLatticeEntry {
  // NOTE: (end - start) does not necessarily == word.size(); word could be an
  // alternate form of what's actually in the original text.
  std::wstring word;
  size_t start, end, filled_end;
  WordLatticeEntry(size_t start) : start(start) {}
  WordLatticeEntry(std::wstring line) {
    // in Ruby this would be word,start,end = *line.split(/\t/)
    std::wstring::iterator first_tab =
      std::find(line.begin(), line.end(), L'\t');
    if (first_tab == line.end())
      throw L"bogus word lattice entry: expected 3 columns but got 1";
    std::wstring::iterator second_tab =
      std::find(first_tab + 1, line.end(), L'\t');
    if (second_tab == line.end())
      throw L"bogus word lattice entry: expected 3 columns but got 2";
    word.resize(first_tab - line.begin());
    std::copy(line.begin(), first_tab, word.begin());
    std::wstring start_str(first_tab+1, second_tab),
                 end_str(second_tab+1, line.end());
    start = wcstol(start_str.c_str(), 0, 10);
    end = wcstol(end_str.c_str(), 0, 10);
    filled_end = end;
  }

  // NOTE: this is different from TextTagger's usual order. Here we order by
  // start so that we can easily find the next entry in a chain.
  bool operator<(size_t rhs_start) const {
    return start < rhs_start;
  }

  bool operator<(const WordLatticeEntry& rhs) const {
    return *this < rhs.start;
  }
};

std::wostream& operator<<(std::wostream& s, const WordLatticeEntry& e) {
  std::wstring word = e.word;
  if (e.end < e.filled_end)
    word.erase(word.end()-1); // erase space added by fill_gaps()
  return (s << word << L"\t" << e.start << L"\t" << e.end << std::endl);
}

class WordLattice {
  std::multiset<WordLatticeEntry> entries;
  friend std::wostream& operator<<(std::wostream& s, const WordLattice& l);
  public:
    typedef std::multiset<WordLatticeEntry>::const_iterator entry_ptr;
    
    void add(const WordLatticeEntry& entry) {
      entries.insert(entry);
    }

    void add(std::wstring entry_line) {
      add(WordLatticeEntry(entry_line));
    }

    void erase_last() {
      entries.erase(--entries.end());
    }

    // Move all end offsets up to the next start, and add a space to the
    // affected entries. This is like TextTagger's :fill-gaps t except that
    // doesn't add space.
    void fill_gaps() {
      for (entry_ptr entry = entries.begin();
           entry != entries.end();
	   ++entry) {
	WordLatticeEntry search(entry->end);
	entry_ptr next = std::lower_bound(entries.begin(), entries.end(), search);
	// const_cast is slightly evil, but it doesn't matter here because
	// we're not modifying anything used by operator<
	WordLatticeEntry& mutable_entry =
	  const_cast<WordLatticeEntry&>(*entry);
	if (next != entries.end() && next->start > entry->end) {
	  mutable_entry.filled_end = next->start;
	  mutable_entry.word += L' ';
	} else {
	  mutable_entry.filled_end = mutable_entry.end;
	}
      }
    }

    size_t max_start() const {
      if (entries.empty()) {
	return 0;
      } else {
        return entries.rbegin()->start;
      }
    }

    entry_ptr first_entry_starting_at(size_t start) const {
      WordLatticeEntry search(start);
      entry_ptr ret = std::lower_bound(entries.begin(), entries.end(), search);
      if (!at_end(ret) && ret->start == start) {
	return ret;
      } else {
	return entries.end();
      }
    }

    entry_ptr next_entry_with_same_start(entry_ptr prev) const {
      entry_ptr next = prev;
      ++next;
      if (prev->start == next->start) {
	return next;
      } else {
	return entries.end();
      }
    }

    bool at_end(entry_ptr e) const {
      return e == entries.end();
    }
};

std::wostream& operator<<(std::wostream& s, const WordLattice& l) {
  s << L"_BEGIN_WORD_LATTICE_" << std::endl;
  for (WordLattice::entry_ptr i = l.entries.begin();
       i != l.entries.end();
       ++i) {
    s << *i;
  }
  s << L"_END_WORD_LATTICE_" << std::endl;
  return s;
}

void find_and_print_term(Vocabulary& vocab, WordLattice* lattice, std::wstring& term, size_t term_start, size_t term_length) {
  std::wstring mod_term = term;
  to_no_dashes(mod_term);
  to_no_caps(mod_term);
  to_normal_space(mod_term);
  std::wstring info;
  if (vocab.look_up(mod_term, info)) {
    if (lattice)
      std::wcout << *lattice;
    print_found_term(mod_term, term_start, term_length, info);
  }
}

// recursively make chains of entries starting at middle and appending to
// prefix, such that the total length of the string is not more than
// max_length, and the total number of lattice entries used to construct the
// string is not more than max_token_count, and call find_and_print_term on
// each such string.
void find_and_print_terms(Vocabulary& vocab, const WordLattice& lattice,
       size_t start, WordLattice& prefix_lattice, const std::wstring& prefix,
       size_t middle, size_t max_length, size_t max_token_count
     ) {
  for ( WordLattice::entry_ptr entry =
          lattice.first_entry_starting_at(middle);
        !lattice.at_end(entry);
        entry = lattice.next_entry_with_same_start(entry)
      ) {
    size_t term_length = prefix.size() + entry->word.size();
    if (term_length <= max_length) {
      prefix_lattice.add(*entry);
      std::wstring term = prefix + entry->word;
      // make a version of the term with no space at the end if we added one
      std::wstring term_no_gap = term;
      size_t term_real_length = entry->end - start;
      if (term_no_gap[term_no_gap.size()-1] == L' ') {
	term_no_gap.erase(term_no_gap.end()-1);
      }
      // look it up and print it
      find_and_print_term(vocab, &prefix_lattice, term_no_gap, start, term_real_length);
      // recurse if we haven't reached the length limits or the end of the
      // lattice
      if (max_token_count > 1 && term_length < max_length && entry->filled_end <= lattice.max_start()) {
        find_and_print_terms(vocab, lattice, start, prefix_lattice, term,
			     entry->filled_end, max_length,
			     max_token_count - 1);
      }
      prefix_lattice.erase_last();
    }
  }
}

int main(int argc, char** argv) {
  if (argc != 2) {
    std::cerr << "USAGE: " << argv[0] << " /path/to/terms.txt" << std::endl;
    return 1;
  }
#ifdef USE_C_STYLE_SETLOCALE
  if (!setlocale(LC_ALL, "en_US.UTF-8")) {
    std::wcerr << "Warning, failed to set locale" << std::endl;
  }
#else
  try {
    std::locale loc("en_US.UTF-8");
    loc.global(loc);
  } catch (std::runtime_error& e) {
    std::wcerr << "Warning, failed to set locale: " << e.what() << std::endl;
  }
#endif
  Vocabulary vocab(argv[1]);
  std::wcerr << "Indexed " << vocab.get_index_size() << " out of " << vocab.get_num_terms() << " terms from " << argv[1] << ". Ready." << std::endl;
  size_t max_term_length = vocab.get_max_term_length();
  std::wstring line;
  while (!std::wcin.eof()) {
    line.clear();
    std::getline(std::wcin, line);
    if (line == L"_BEGIN_WORD_LATTICE_") {
      // read in the whole lattice
      WordLattice lattice;
      while (!std::wcin.eof()) {
	line.clear();
	std::getline(std::wcin, line);
	if (line == L"_END_WORD_LATTICE_")
	  break;
	try {
	  lattice.add(line);
	} catch (wchar_t const* msg) {
	  std::wcerr << msg << std::endl;
	}
      }
      lattice.fill_gaps();
      // scan through potential starting points and build chains of lattice
      // entries to look up in vocab
      size_t max_start = lattice.max_start();
      size_t sane_max_term_length = // can't let this get big; search explodes
        (max_term_length < 100 ? max_term_length : 100);
      WordLattice prefix_lattice;
      for (size_t start = 0; start <= max_start; ++start) {
//	std::wcerr << L"start=" << start << std::endl;
	find_and_print_terms(vocab, lattice, start, prefix_lattice, L"", start,
			     sane_max_term_length,
			     20 // max_token_count
			     );
      }
    } else if (line == L"_EXACT_") {
      // match the whole following line exactly (no variants, no character
      // offsets, just a plain old table lookup)
      line.clear();
      std::getline(std::wcin, line);
      std::wstring info;
      if (vocab.look_up(line, info))
	std::wcout << info << std::endl;
    } else {
      // For each substring of the input line short enough to be a term and
      // bounded by word boundaries...
      for (std::wstring::const_iterator term_start = line.begin();
	   term_start != line.end();
	   ++term_start) {
	if (term_start == line.begin() ||
	    ((!iswalnum(*(term_start-1))) &&
	     (!is_perl_space(*term_start)))) {
	  // The start of the term is at a word boundary.
	  for (size_t term_length = 1;
	       term_length <= max_term_length &&
	       term_start + term_length <= line.end();
	       ++term_length) {
	    if (term_start + term_length == line.end() ||
		((!iswalnum(*(term_start + term_length))) &&
		 (!is_perl_space(*(term_start + term_length - 1))))) {
	      // The end of the term is at a word boundary.
	      std::wstring term = line.substr(term_start - line.begin(), term_length);
	      find_and_print_term(vocab, NULL, term, term_start - line.begin(), term_length);
	    }
	  }
	}
      }
    }
    // Print a blank line to indicate that we're done processing this line of
    // input.
    std::wcout << std::endl << std::flush;
  }
  return 0;
}
