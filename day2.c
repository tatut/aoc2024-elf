#include "aoc.h"
#include <stdbool.h>

long sign(long val) {
  return val < 0
    ? -1
    : (val > 0
       ? 1
       : 0);
}

bool monotonic(long* nums, int count, long skip) {
  long dir = 0;
  bool first = true;
  long last;
  for(int i=0;i<count;i++) {
    if(i!=skip) {
      if(first) { first = false; last = nums[i]; }
      else {
        if(dir == 0) { dir = sign(last-nums[i]); }
        if(dir == 0) return false;
        long diff = last-nums[i];
        if(sign(diff) != dir) return false;
        if(labs(diff) > 3) return false;
        last = nums[i];
      }
    }
  }
  return true;
}


aoc_main({
    size_t len;
    char *in = input("day2.txt",&len);
    long p1=0;
    long p2=0;
    long *nums = malloc(sizeof(long)*20);
    lines_each(in, line, {
        long l=0;
        char* after;
        long n = number(line, &after);
        while(n != -1) {
          nums[l++] = n;
          n = number(after, &after);
        }
        if(monotonic(nums,l,-1)) p1++;
        for(int skip=-1; skip<l;skip++) {
          if(monotonic(nums, l, skip)) { p2++; break; }
        }
      });
    printf("Part1: %ld\nPart2: %ld\n", p1, p2);
  });
