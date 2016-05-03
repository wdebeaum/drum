/* bloom_filter - a simple Bloom filter implementation for std::wstring keys,
 * based on MurmurHash3.
 * William de Beaumont
 * 2014-08-19
 */

#include <cmath>
#include <cstdlib>
#include <vector>
#include <string>
#include "MurmurHash3.h"
#include "bloom_filter.h"

const double LN_2 = std::log(2);
const double M_DENOMINATOR = LN_2 * LN_2;

HashFunction::HashFunction() : seed(std::rand()) {}

uint32_t HashFunction::operator()(std::wstring key) const {
  uint32_t out;
  MurmurHash3_x86_32(key.data(), sizeof(wchar_t) * key.size(), seed, &out);
  return out;
}

std::vector<size_t> BloomFilter::indexes(std::wstring key) const {
  std::vector<size_t> ret(hash_fns.size());
  for (size_t i = 0; i < hash_fns.size(); ++i)
    ret[i] = hash_fns[i](key) % bits.size(); // FIXME should use std::uniform_int_distribution to avoid hashing to lower indexes slightly more
  return ret;
}
  
BloomFilter::BloomFilter(size_t estimated_num_keys, // n
			 double false_positive_rate) // p
: bits(// optimal size m = - n ln p / (ln 2)^2
       -static_cast<double>(estimated_num_keys) *
	 std::log(false_positive_rate) /
       M_DENOMINATOR,
       false),
  hash_fns(// optimal # hash fns k = m ln 2 / n
  	   static_cast<double>(bits.size()) * LN_2 /
           static_cast<double>(estimated_num_keys))
{}


void BloomFilter::add_key(std::wstring key) {
  std::vector<size_t> is = indexes(key);
  for (size_t i = 0; i < is.size(); ++i)
    bits[is[i]] = true;
}

bool BloomFilter::may_contain_key(std::wstring key) const {
  std::vector<size_t> is = indexes(key);
  for (size_t i = 0; i < is.size(); ++i)
    if (!bits[is[i]])
      return false;
  return true;
}

