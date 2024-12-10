#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <time.h>

#define FILENAME "day9.txt"

size_t total_len(char *d) {
  size_t l = 0;
  while(d[0] != 0) {
    l += (size_t) (d[0] - 48); d++;
  }
  return l;
}

/* Expand input to array of file ids at position.
 * if position is free space, the id is -1 */
void expand(char *input, long *output) {
  long p=0;
  long id=0;
  char file = 1;
  while(input[0] != 0) {
    int len = input[0] - 48;
    long v = file ? id++ : -1;
    for(int i=0;i<len;i++) output[p++] = v;
    input++;
    file = !file;
  }
}

// move single file blocks to free spaces
void move_blocks(long *blocks, size_t len) {
  int l=0;
  int r=len-1;
  while(l<r) {
    if(blocks[r]==-1) { r--; continue; }
    if(blocks[l]==-1 && blocks[r]!=-1) {
      blocks[l] = blocks[r];
      blocks[r] = -1;
      l++; r--;
    } else {
      l++; // no free, increment left pos
    }
  }
}

long checksum(long *blocks, size_t len) {
  long cs = 0;
  for(size_t i = 0; i < len; i++) {
    if(blocks[i] != -1) cs += i*blocks[i];
  }
  return cs;
}

struct space {
  size_t pos;
  size_t len;
  struct space* prev;
  struct space* next;
};

int comp(struct space* l, struct space* r) {
  return (l->pos < r->pos
          ? -1
          : (l->pos > r->pos
             ? 1
             : 0));
}

struct space *new_space() { return malloc(sizeof(struct space)); }

struct space* spaces(char *input, size_t len) {
  size_t num_spaces = (size_t) len/2;
  struct space* head = NULL;
  struct space* cur = NULL;
  size_t pos=0;

  for(size_t i=0; i<len; i++) {
    size_t l = input[i]-48;
    if(i%2==0) pos += l;
    else {
      if(cur == NULL) {
        head = new_space();
        cur = head;
      } else {
        struct space *ns = new_space();
        ns->prev = cur;
        cur->next = ns;
        cur = ns;
      }
      cur->pos = pos;
      cur->len = l;
      pos += l;
    }
  }
  return head;
}

struct space* find_space(struct space *s, size_t before, size_t len){
  while (s != NULL && s->pos < before) {
    if (s->len >= len)
      return s;
    s = s->next;
  }
  return NULL;
}

void debug(long *blocks, size_t len) {
    printf("|| ");
    for (size_t i = 0; i < len; i++) {
        if(blocks[i]==-1) printf(".");
        else printf("%ld", blocks[i]);
    }
    printf(" ||\n");
}

void debug_space(struct space *sp) {
  printf(" space at %ld, len %ld", sp->pos, sp->len);
  if (sp->prev)
    printf(" (PREV: %ld, len %ld)", sp->prev->pos, sp->prev->len);
  if (sp->next)
    printf(" (NEXT: %ld, len %ld)", sp->next->pos, sp->next->len);
  printf("\n");
}

void debug_spaces(struct space *sp) {
  while (sp != NULL) {
      debug_space(sp);
      sp = sp->next;
  }
}

/* Try to move files intact to earlier spaces.
 * Go backwards through blocks.
 */
void move_files(struct space *init_spaces, long *blocks, size_t blocks_len) {
  long *blocks2 = malloc(sizeof(long) * blocks_len);
  memcpy(blocks2, blocks, sizeof(long)*blocks_len);
  struct space *spaces = init_spaces;
  size_t at = blocks_len - 1;
  size_t min = spaces->pos + spaces->len;
  while (at > min) {
      // move to end of file
      while (blocks2[at] == -1) at--;
      size_t file_end = at;
      long file = blocks2[file_end];

      // move to start of file
      while (blocks2[at-1] == file) at--;
      size_t file_start = at;

      size_t file_len = file_end - file_start + 1;
      struct space *s = find_space(spaces, file_start, file_len);
      if (s) {
          // found a space to move this file
          for (size_t i = 0; i < file_len; i++) {
              blocks[s->pos + i] = file;
              blocks[file_start + i] = -1;
          }
          // replace this space in the linked list
          if (s->len > file_len) {
              size_t remaining_space = s->len - file_len;
              struct space *ns = new_space();
              ns->len = remaining_space;
              ns->pos = s->pos + file_len;
              if (s == spaces) {
                  // replacing the 1st space
                  spaces = ns;
                  ns->next = s->next;
                  s->next->prev = ns;
              } else {
                  s->prev->next = ns;
                  ns->prev = s->prev;
                  ns->next = s->next;
                  s->next->prev = ns;
              }
              ns->next = s->next;
          } else {
              if (s->prev == NULL) {
                  spaces = s->next;
                  spaces->prev = NULL;
              } else {
                  struct space *p = s->prev;
                  struct space *n = s->next;
                  p->next = n;
                  n->prev = p;

              }
          }
          free(s);
      }
      at--;
  }
  free(blocks2);
}


int main(int arvc, char **argv) {
  struct stat b;

  clock_t start = clock();
  stat(FILENAME, &b);
  size_t sz = b.st_size;
  char* input = malloc(sz);
  FILE* f = fopen(FILENAME, "r");
  fread(input, b.st_size, 1, f);
  input[sz-1] = 0;

  size_t total = total_len(input);
  long* part1 = malloc(sizeof(long)*total);
  long* part2 = malloc(sizeof(long)*total);

  expand(input, part1);

  // make copy to use in part2
  memcpy(part2, part1, sizeof(long)*total);

  move_blocks(part1, total);
  printf("Part1: %ld\n", checksum(part1, total));

  struct space* sps = spaces(input, sz-1);
  struct space* here = sps;
  move_files(sps, part2, total);
  printf("Part2: %ld\n", checksum(part2, total));

  free(part1);
  free(part2);
  free(input);

  clock_t stop = clock();
  printf("Took %lfs\n", (double) (stop-start) / (double)CLOCKS_PER_SEC);
  exit(0);
}
