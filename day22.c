#include "aoc.h"

#define MOD 16777216

long next(long in) {
  long s1 = (in ^ (in*64)) % MOD;
  long s2 = (s1 ^ (s1>>5)) % MOD;
  long s3 = (s2 ^ (s2*2048)) % MOD;
  return s3;
}

long nth_next(long in, size_t nth) {
  long out = in;
  while(nth > 0) {
    out = next(out);
    nth--;
  }
  return out;
}

long* nexts(long in) {
  long* out = malloc(2001*sizeof(long));
  out[0] = in;
  for(size_t i=1; i<2001; i++) {
    out[i] = next(out[i-1]);
  }
  return out;
}

void part1(char *in) {
  long secret;
  char *after;

  long sum = 0;
  lines_each(in, line, {
      secret = number(line, &after);
      sum += nth_next(secret, 2000);
    });
  printf("Part1: %ld\n", sum);
}

long bananas(long *monkey, int a,int b,int c, int d) {
  for(size_t i=0; i<1996; i++) {
    long av = monkey[i+0] % 10;
    long bv = monkey[i+1] % 10;
    long cv = monkey[i+2] % 10;
    long dv = monkey[i+3] % 10;
    long fv = monkey[i+4] % 10;
    if((bv - av) == a && (cv - bv) == b && (dv - cv) == c && (fv - dv) == d)
      return fv;
  }
  return 0;
}

void part2(char *in) {
  size_t lines = lines_count(in);
  char *after;

  // generate all numbers (initial+2k nexts) for each monkey
  long** monkeys = malloc(sizeof(long*)*lines);
  size_t i=0;
  lines_each(in, line, {
      monkeys[i++] = nexts(number(line, &after));
    });

  long best=0;
  for(int a=-9; a<10;a++) {
    printf("a: %d\n", a);
      for(int b=-9; b<10;b++) {
          for(int c=-9; c<10;c++) {
              for(int d=-9; d<10;d++) {
                long bs=0;
                for(int m=0;m<lines;m++) {
                  bs += bananas(monkeys[m], a,b,c,d);
                }
                if(bs > best) { best = bs; printf("best so far: %ld\n", best); }
              }
          }
      }
  }
  printf("Part2: %ld\n", best);
  // should free() all the stuff, but we are exiting anyway
}
aoc_main({
  size_t sz;
  char *in = input("day22.txt", &sz);
  part1(in);
  part2(in);
})
