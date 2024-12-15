#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdbool.h>
#include <time.h>
#include <string.h>

#define panic(args...)                                                         \
  {                                                                            \
    printf(args);                                                              \
    printf("\n");                                                              \
    exit(1);                                                                   \
  }

// grid from file, each row ends with \n
struct Grid {
  size_t w;
  size_t h;
  char *data;
};

// return next occurence of ch in input, or end of input
char *find(char *input, char ch) {
  char *out = input;
  while(*out != ch && *out != 0) out++;
  return out;
}

bool looking_at(char *start, char *expected) {
  char *input = start;
  while(*expected != 0) {
    if(*input == 0) return false;
    if(*input != *expected) return false;
    input++;
    expected++;
  }
  return true;
}

char* input(const char *file, size_t *len) {
  struct stat b;
  stat(file, &b);
  FILE* f = fopen(file, "r");
  char* in = (char*) malloc(b.st_size+1);
  fread(in, b.st_size, 1, f);
  *len = b.st_size;
  in[b.st_size] = 0;
  fclose(f);
  return in;
}

struct Grid *grid(const char *file) {
  size_t len;
  char* data = input(file, &len);
  size_t w = find(data,'\n') - data;
  size_t h = len / (w+1);
  struct Grid* out = (struct Grid*) malloc(sizeof(struct Grid));
  out->w = w;
  out->h = h;
  out->data = data;
  return out;
}

struct Grid *grid_from_string(char *data) {
  size_t len = strlen(data);
  size_t w = find(data,'\n') - data;
  size_t h = len / (w+1);
  struct Grid* out = (struct Grid*) malloc(sizeof(struct Grid));
  out->w = w;
  out->h = h;
  out->data = data;
  return out;
};

struct Grid *grid_new(size_t w, size_t h) {
  char *data = (char *) calloc(((w+1)*h), sizeof(char));
  struct Grid *g = (struct Grid*) malloc(sizeof(struct Grid));
  g->w = w;
  g->h = h;
  g->data = data;
  return g;
}

struct Grid *grid_copy(struct Grid *in) {
  struct Grid *out = (struct Grid *)malloc(sizeof(struct Grid));
  out->w = in->w;
  out->h = in->h;
  out->data = (char*)malloc(sizeof(char)*(in->w+1)*in->h);
  memcpy(out->data, in->data, sizeof(char)*(in->w+1)*in->h);
  return out;
}

void grid_free(struct Grid* g) {
  free(g->data);
  free(g);
}

#define grid_each(g, x, y, val, body)                                          \
  for (size_t y = 0; y < g->h; y++) {                                          \
    for (size_t x = 0; x < g->w; x++) {                                        \
      char val = grid_at(g, x, y);                                             \
      body                                                                     \
    }                                                                          \
  };

#define grid_each_pos(g, x, y, body)                                           \
  for (size_t y = 0; y < g->h; y++) {                                          \
    for (size_t x = 0; x < g->w; x++) {                                        \
      body                                                                     \
    }                                                                          \
  }

#define grid_neighbors(g, x, y, nx, ny, val, body)                             \
  size_t nx;                                                                   \
  size_t ny;                                                                   \
  char val;                                                                    \
  nx = x - 1;                                                                  \
  ny = y;                                                                      \
  val = grid_at(g, nx, ny);                                                    \
  if (val != 0)                                                                \
    body;                                                                      \
  nx = x + 1;                                                                  \
  val = grid_at(g, nx, ny);                                                    \
  if (val != 0)                                                                \
    body;                                                                      \
  nx = x;                                                                      \
  ny = y - 1;                                                                  \
  val = grid_at(g, nx, ny);                                                    \
  if (val != 0)                                                                \
    body;                                                                      \
  ny = y + 1;                                                                  \
  val = grid_at(g, nx, ny);                                                    \
  if (val != 0)                                                                \
    body;

// All neighbors 4, even outside bounds (val==0 then)
#define grid_neighbors_all(g, x, y, nx, ny, val, body)                         \
  size_t nx;                                                                   \
  size_t ny;                                                                   \
  char val;                                                                    \
  nx = x - 1;                                                                  \
  ny = y;                                                                      \
  val = grid_at(g, nx, ny);                                                    \
  body;                                                                        \
  nx = x + 1;                                                                  \
  val = grid_at(g, nx, ny);                                                    \
  body;                                                                        \
  nx = x;                                                                      \
  ny = y - 1;                                                                  \
  val = grid_at(g, nx, ny);                                                    \
  body;                                                                        \
  ny = y + 1;                                                                  \
  val = grid_at(g, nx, ny);                                                    \
  body;




#define grid_find(g, val, x, y)                                                \
  for (size_t gfx = 0; gfx < g->w; gfx++) {                                    \
    for (size_t gfy = 0; gfy < g->h; gfy++) {                                  \
      if (grid_at(g, gfx, gfy) == val) {                                       \
        x = gfx;                                                               \
        y = gfy;                                                               \
        goto grid_find_found;                                                  \
      }                                                                        \
    }                                                                          \
  }                                                                            \
  printf("ERROR char not found in grid: %d\n", val);                           \
  grid_find_found:

#define grid_idx(g, x, y) (y * g->w + x)
#define grid_size(g) (g->w * g->h)


char grid_at(struct Grid* g, size_t x, size_t y) {
  return (x < 0 || y < 0 || x >= g->w || y >= g->h)
    ? 0
    : g->data[(y * (g->w + 1)) + x]; // don't use grid_idx due to \n
}

// Set value at grid position (which *MUST* be inside the grid)
void grid_set(struct Grid* g, size_t x, size_t y, char ch) {
  g->data[(y * (g->w + 1)) + x] = ch; // don't use grid_idx due to \n
}

bool is_digit(char c) { return c >= 48 && c <= 57; }

// Read a positive number, if number is not found it
// is set to -1
long number(char* ptr, char** after) {
  if(*ptr==0 || *ptr=='\n') return -1;
  while(!(*ptr == 0) && !is_digit(*ptr)) ptr++;
  if(*ptr == 0) return -1;
  char *end = ptr;
  while(is_digit(*end)) end++;
  char old_end = *end;
  *end = 0;
  long res = atol(ptr);
  *end = old_end;
  *after = end;
  return res;
}

// Read a number at position (which must be found here)
// if first char is '-' then number is negative
long number_neg(char *ptr, char **after) {
  if(*ptr != '-' && !is_digit(*ptr)) panic("Expected - or a digit when parsing number");
  char *end = ptr+1;
  while(is_digit(*end)) end++;
  char old_end = *end;
  *end = 0;
  long res = atol(ptr);
  *end = old_end;
  *after = end;
  return res;
}

size_t lines_count(char* input) {
  int c=0;
  while(*input != 0) {
    if(*input == '\n') c++;
    input++;
  }
  return c;
}


#define lines_each(input, line_idx, line, body)                          \
  char *line = input;                                                          \
  bool _line_done = false;                                                     \
  size_t line_idx = 0; \
  while (*line != 0 && _line_done == false) {                                  \
    char *_line_end = find(line, '\n');                                        \
    char _old_line_end = *_line_end;                                           \
    if (*_line_end == 0)                                                       \
      _line_done = true;                                                       \
    else                                                                       \
      *_line_end = 0;                                                          \
    body *_line_end = _old_line_end;                                           \
    line = _line_end + 1;                                                      \
    line_idx++; \
  }















clock_t started;

void time_start() { started = clock(); }
void time_end() {
  clock_t ended = clock();
  printf("Took %ldms\n",  (ended-started) * 1000 / CLOCKS_PER_SEC);
}

#define aoc_main(body) int main(int __argc, char **__argv) { time_start(); { body }; time_end(); return 0; }
