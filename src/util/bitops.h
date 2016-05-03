/*
 * bitops.h : Bit operations
 *
 * George Ferguson, ferguson@cs.rochester.edu,  8 Apr 1995
 * Time-stamp: <Wed Sep 24 13:25:37 EDT 2003 ferguson>
 */

#ifndef _bitops_h_gf
#define _bitops_h_gf

#define BITSET(B,N)	(B) |= (1 << (N))
#define BITCLR(B,N)	(B) &= ~(1 << (N))
#define BITTST(B,N)	((B) & (1 << (N)))

#endif
