#include "aoc.h"

// Maybe read a number if *ptr starts with one contains endch after digits
bool maybe_number(char *ptr, char endch, long *number, char **after) {
  char *end=ptr;
  if(!is_digit(*ptr)) return false;
  while(is_digit(*end)) end++;
  if(*end == endch) {
    *after = end+1;
    *number = atol(ptr);
    return true;
  }
  return false;
}

aoc_main({
    size_t len;
    char *in = input("day3.txt", &len);
    char *at = in;
    long p1 = 0;
    long p2 = 0;
    bool enabled = true;
    while(*in != 0) {
      if(looking_at(in, "don't()")) { enabled=false; in += 6; }
      else if(looking_at(in, "do()")) { enabled=true; in += 3; }
      else if(looking_at(in, "mul(")) {
        char *num1_start = in+4;
        char *after;
        long num1;
        long num2;
        if(maybe_number(num1_start, ',', &num1, &after)) {
          if(maybe_number(after, ')', &num2, &after)) {
            //if(enabled) printf("[X]"); else printf("[ ]"); printf(" %ld * %ld\n", num1, num2);
            p1 += (num1 * num2);
            if(enabled) p2 += (num1 * num2);
            in = after;
            continue;
          }
        }
      }
      in++;
    }
    printf("Part1: %ld\nPart2: %ld\n", p1, p2);
  });
