#include "aoc.h"
#include <stdbool.h>


size_t corners(struct Grid *g, size_t x, size_t y) {
  char type = grid_at(g, x, y);
  bool
    u = grid_at(g, x, y-1)==type,
    r = grid_at(g, x+1, y)==type,
    d = grid_at(g, x, y+1)==type,
    l = grid_at(g, x-1, y)==type,
    ul = grid_at(g, x-1, y-1)==type,
    ur = grid_at(g, x+1, y-1)==type,
    dl = grid_at(g, x-1, y+1)==type,
    dr = grid_at(g, x+1, y+1)==type;

  int sides = 0;
  //0X0
  //XX0
  //000
  //
  // 0
  //0X

  if (u && l && !ul) sides++;
  if (!u && !l) sides++; // ┌ inside
  if (u && r && !ur) sides++; // ┐
  if (!u && !r) sides++;
  if (d && l && !dl) sides++;
  if (!d && !l) sides++;
  if (d && r && !dr) sides++;
  if (!d && !r) sides++; // ┌ ┐ └ ┘

  return sides;
}
/* Survey the plot at the given position.
   Walks through all reachable positions. */
void survey(struct Grid *g, bool *used, char type, size_t x, size_t y,
            size_t *perimeter, size_t *area, size_t *sides) {
  used[grid_idx(g,x,y)] = true;

  grid_neighbors_all(g, x, y, nx, ny, nt, {
      if(nt != type) {
        *perimeter = *perimeter + 1;
      } else if(!used[grid_idx(g,nx,ny)]) {
        // continue surveying
        survey(g, used, type, nx, ny, perimeter, area, sides);
        *area = *area + 1;
      }
    });
  *sides = *sides + corners(g, x, y);
}

// move forward in direction
void fwd(int x, int y, int dir, int *nx, int *ny) {
  switch(dir) {
  case 0: *nx = x; *ny = y - 1; break;
  case 1: *nx = x + 1; *ny = y; break;
  case 2: *nx = x; *ny = y + 1; break;
  case 3: *nx = x - 1; *ny = y; break;
  }
}

// position for right side of me in dir
void right(int x, int y, int dir, int *rx, int *ry) {
  switch(dir) {
  case 0: *rx = x+1; *ry = y; break;
  case 1: *rx = x; *ry = y+1; break;
  case 2: *rx = x-1; *ry = y; break;
  case 3: *rx = x; *ry = y-1; break;
  }
}

int turn(int dir) { return (dir+1) % 4; }


// FIXME: doesn't work for real input, some cornercase not handled
void walk(struct Grid *g, int startx, int starty, char type, int dir, size_t *sides) {
  // walk forward until we need to turn
  int x = startx;
  int y = starty;
  int rx, ry, nx, ny;
  int gas = 5000;

  while(gas) {
    //printf("at x: %d, y: %d, facing %d\n", x, y, dir);
    fwd(x, y, dir, &nx, &ny);
    //if(nx==startx && ny==starty) return;
    right(nx, ny, dir, &rx, &ry);
    if((grid_at(g, rx, ry)!=type)) {
       // >XX
       // X.XX  <- remains unvisited
       // XXXXX
       //

      //
      // > #
      // #.
      //

      // move, turn right and move
      //printf("turn right at x:%d, y: %d\n", x, y);
      dir = (dir+1)%4;
      fwd(x, y, dir, &x, &y);
      *sides = *sides + 1;
      x = rx; y = ry;
      if(x==startx && y==starty) return;
    } else if(grid_at(g, nx, ny)==type) {
      // turn left without moving
      //printf("turn left x: %d, y: %d\n", x, y);
      dir = (dir+3)%4;
      *sides = *sides + 1;
     } else {
      x = nx;
      y = ny;
    }

    gas--;
  }
  printf("couldn't find sides for start x: %d, y: %d, type %c\n", startx, starty, type);
  panic("Out of gas");
}


aoc_main({
    struct Grid* g = grid("day12.txt");
    size_t sz = grid_size(g);
    // Track which places have been used when searching for the next plot
    bool *used = malloc(sizeof(bool)*sz);
    // iterate positions left to right, top to bottom, so we always land on the top-left pos
    // of a plot.
    size_t area;
    size_t perimeter;
    size_t sides;
    long p1 = 0;
    long p2 = 0;
    grid_each(g, x, y, type, {
        if(!used[grid_idx(g,x,y)]) {
          area = 1;
          perimeter = 0;
          sides = 0;
          survey(g, used, type, x, y, &perimeter, &area, &sides);
          //printf("P1: A region of %c plants with price %ld * %ld = %ld  (x:%ld,y:%ld)\n", type, area, perimeter, area*perimeter, x, y);

          // FIXME: non working version that tries to walk around the perimeter
          // start above first place, walking left, count turns
          //if(area == 1) sides = 4;
          //else walk(g, x, y - 1, type, 1, &sides);

          //printf("P2: A region of %c plants with price %ld * %ld = %ld  (x:%ld,y:%ld)\n", type, area, sides, area*sides, x, y);

          p1 += area*perimeter;
          p2 += area*sides;
        }
      });
    printf("Part1: %ld\nPart2: %ld\n", p1, p2);
  });
