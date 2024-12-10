#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdbool.h>
#include <time.h>

#define panic(X)                                                               \
  {                                                                            \
    printf(X);                                                                 \
    printf("\n");                                                              \
    exit(1);                                                                   \
  }

// grid from file, each row ends with \n
struct Grid {
  size_t w;
  size_t h;
  char *data;
};

char *find(char *input, char ch) {
  char *out = input;
  while(*out != ch) out++;
  return out;
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


clock_t started;

void time_start() { started = clock(); }
void time_end() {
  clock_t ended = clock();
  printf("Took %ldms\n",  (ended-started) * 1000 / CLOCKS_PER_SEC);
}
