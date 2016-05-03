/* bloom_filter - a simple Bloom filter implementation for std::wstring keys,
 * based on MurmurHash3.
 * William de Beaumont
 * 2014-08-19
 */

#include <vector>
#include <string>
//#include <cstdint>
#include <stdint.h>

class HashFunction {
  uint32_t seed;
  public:
    HashFunction();
    uint32_t operator()(std::wstring key) const;
};

class BloomFilter {
  std::vector<bool> bits; // size is m
  std::vector<HashFunction> hash_fns; // size is k
  std::vector<size_t> indexes(std::wstring key) const;
  public:
    BloomFilter(size_t estimated_num_keys, // n
                double false_positive_rate); // p
    void add_key(std::wstring key);
    bool may_contain_key(std::wstring key) const;
};

