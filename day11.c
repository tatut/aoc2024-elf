#include "aoc.h"
#include <stdint.h>
#include <math.h>

// A hashtable with predefined amount of space
struct entry {
  uint64_t number;
  long count; // if -1, this is unused
};

#define NENTRIES 16

struct bucket {
  struct entry entries[16];
};

#define NBUCKETS 512

struct ht {
  struct bucket buckets[NBUCKETS];
};

// get hash for number (from https://stackoverflow.com/a/12996028)
// supposedly a "good" hash function for 64bit integers
uint64_t hash(uint64_t x) {
    x = (x ^ (x >> 30)) * UINT64_C(0xbf58476d1ce4e5b9);
    x = (x ^ (x >> 27)) * UINT64_C(0x94d049bb133111eb);
    x = x ^ (x >> 31);
    return x;
}

void ht_put(struct ht* t, uint64_t n, long count) {
  struct bucket *b = &t->buckets[hash(n) % NBUCKETS];
  size_t first_free = -1;
  for(size_t i=0;i<NENTRIES; i++) {
    if(b->entries[i].count != -1 && b->entries[i].number == n) {
      b->entries[i].count = count;
      return;
    }
    if(first_free == -1 && b->entries[i].count == -1) first_free = i;
  }
  if(first_free == -1) panic("No space in bucket");
  b->entries[first_free].number = n;
  b->entries[first_free].count = count;
}

long ht_get(struct ht* t, uint64_t n) {
  struct bucket *b = &t->buckets[hash(n) % NBUCKETS];
  for(size_t i=0;i<NENTRIES; i++) {
    if(b->entries[i].count != -1 && b->entries[i].number == n) return b->entries[i].count;
  }
  return -1;
}

void ht_upd(struct ht *t, uint64_t n, long by) {
  long existing = ht_get(t, n);
  if(existing==-1) {
    ht_put(t, n, by < 0 ? -1 : by);
  } else {
    ht_put(t, n, existing + by);
  }
}

void ht_clear(struct ht *t) {
  for(size_t b=0;b<NBUCKETS;b++) {
    for(size_t e=0;e<NENTRIES;e++) {
      t->buckets[b].entries[e].count = -1;
    }
  }
}

struct ht *ht_new() {
  struct ht *t = malloc(sizeof(struct ht));
  ht_clear(t);
  return t;
}

uint8_t numdigits(uint64_t n) {
  uint8_t d = 1;
  while(n > 9) { d++; n /= 10; }
  return d;
}

void split(uint64_t n, uint8_t d, uint64_t *l, uint64_t *r) {
  *l = n;
  for(uint8_t i=0;i<d;i++) {
    *l /= 10;
  }
  *r = n % (uint64_t) pow(10, d);
}

void blink(struct ht *from, struct ht *to) {
  ht_clear(to);
  for(size_t bn=0;bn<NBUCKETS;bn++) {
    for(size_t e=0;e<NENTRIES;e++) {
      long c = from->buckets[bn].entries[e].count;
      if(c != -1) {
        uint64_t n = from->buckets[bn].entries[e].number;
        if(n == 0) {
          // replace 0 with 1
          ht_put(to, 1, c);
        } else {
          uint8_t d = numdigits(n);
          if(d % 2 == 0) {
            // even number of digits, split
            uint64_t l, r;
            split(n, d/2, &l, &r);
            ht_upd(to, l, c);
            ht_upd(to, r, c);
          } else {
            ht_upd(to, n*2024, c);
          }
        }
      }
    }
  }
}

void ht_print(struct ht *t) {
  for(size_t b=0;b<NBUCKETS; b++) {
    for(size_t e=0;e<NENTRIES; e++) {
      struct entry at = t->buckets[b].entries[e];
      if(at.count != -1) {
        printf("%llu => %ld\n", at.number, at.count);
      }
    }
  }
}


long count(struct ht *t) {
  long sum = 0;
  for(size_t b=0;b<NBUCKETS;b++) {
    for(size_t e=0;e<NENTRIES;e++) {
      long c = t->buckets[b].entries[e].count;
      if(c>0) sum += c;
    }
  }
  return sum;
}

void data(struct ht *t) {
  ht_put(t, 4022724, 1);
  ht_put(t, 951333, 1);
  ht_put(t, 0, 1);
  ht_put(t, 21633, 1);
  ht_put(t, 5857, 1);
  ht_put(t, 97, 1);
  ht_put(t, 702, 1);
  ht_put(t, 6, 1);
}

void sample(struct ht *t) {
  ht_put(t, 125, 1);
  ht_put(t, 17, 1);
}

aoc_main({
    struct ht *t1 = ht_new();

    data(t1);
    struct ht *t2 = ht_new();
    for(int i=1;i<=80; i++) {
     if(i%2 == 1) blink(t1, t2);
     else blink(t2, t1);
     if(i==25) printf("Part1: %ld\n", count(t2));
     if(i==75) printf("Part2: %ld\n", count(t2));
    }
  });
