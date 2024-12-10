#include <stdio.h>
#include <stdlib.h>
#include "aoc.h"

#define FILENAME "day7.txt"

bool calc1(long result, long acc, long* rest) {
  if(result<acc) return false;
  long next = *rest;
  if(next==-1) return result==acc;
  return calc1(result, next*acc, rest+1) || calc1(result, next+acc, rest+1);
}

long combine(long n1, long n2) {
  long out = n1*10;
  long n = n2;
  while(n > 9) { out *= 10; n /= 10; }
  return out+n2;
}

bool calc2(long result, long acc, long* rest) {
  if(result<acc) return false;
  long next = *rest;
  if(next==-1) return result==acc;
  return calc2(result, next*acc, rest+1)
    || calc2(result, next+acc, rest+1)
    || calc2(result, combine(acc,next), rest+1);
}


int main(int argc, char **argv) {
  time_start();
  size_t len;
  char* in = input(FILENAME, &len);

  long *nums = malloc(sizeof(long)*64);
  long p1 = 0;
  long p2 = 0;

  // find next line
  char* line_start = in;
  char* line_end = in;
  while(*line_start != 0) {
    line_end = line_start;
    while(*line_end != '\n') line_end++;
    *line_end = 0;

    // parse line: Result: N1 N2 ... Nn
    char *st = line_start;
    long result = number(st, &st);
    long first = number(st, &st);
    long i = 0;
    for(long n = number(st, &st); n>=0;) {
      nums[i++] = n;
      n = number(st, &st);
    }
    nums[i] = -1; // mark end
    if(calc1(result, first, nums)) {
      p1 += result;
    }
    if(calc2(result, first, nums)) {
      p2 += result;
    }
    line_start = line_end + 1;

  }
  printf("Part1: %ld\nPart2: %ld\n", p1, p2);
  time_end();
  free(in);
}
