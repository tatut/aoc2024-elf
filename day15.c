#include "aoc.h"
#include "raylib.h"
#include "colors.h"
#include <string.h>
#include <unistd.h>

// size of grid cell in pixels
#define PX 10

const char *dirs = "^>v<";

void draw(struct Grid *g, int dir) {
  BeginDrawing();
  ClearBackground(BLACK);
  grid_each(g, x, y, val, {
      int cx = x*PX+PX/2;
      int cy = y*PX+PX/2;
      if(val == '@') {
        DrawRectangle(x*PX, y*PX, PX, PX, RED);
        int lx=cx;
        int ly=cy;
        switch(dir) {
        case 0: ly = cy - PX/2; break;
        case 1: lx = cx + PX/2; break;
        case 2: ly = cy + PX/2; break;
        case 3: lx = cx - PX/2; break;
        }
        DrawLine(cx, cy, lx, ly, WHITE);
      } else if(val != '.') {
        Color c;
        switch(val) {
        case '[': c=GREEN; break;
        case ']': c=BLUE; break;
        default: c=LIGHTGRAY; break;
        }
        DrawRectangle(x*PX,y*PX,PX,PX,c);
      }
    });
  EndDrawing();
}

void fwd(int x, int y, int dir, int *nx, int *ny) {
  switch(dir) {
  case 0: *nx = x; *ny = y-1; break;
  case 1: *nx = x+1; *ny = y; break;
  case 2: *nx = x; *ny = y+1; break;
  case 3: *nx = x-1; *ny = y; break;
  default: panic("No such direction");
  }
}

// move what into x,y position, shifting any items into dir
// returns true if we  moved, false if we failed
bool move(struct Grid *g, char what, int fromx, int fromy, int dir) {
  int tox, toy;
  fwd(fromx, fromy, dir, &tox, &toy);

  char at = grid_at(g, tox, toy);

  if(at == '#')
    // can't move into walls
    return false;
  if(at == 'O' && !move(g, at, tox, toy, dir))
    // box here we failed to move it recursively
    return false;
  else if(at != 'O' && at != '.') {
    panic("Unrecognized %c(%d,%d)", at, tox, toy);
  }

  // no problem to moving
  grid_set(g, fromx, fromy, '.');
  grid_set(g, tox, toy, what);
  return true;
}

// recursively check if movement can be done, without doing it
bool can_move(struct Grid *g, char what, int fromx, int fromy, int dir) {
  if(what == '.') return true;
  if(what == ']') {
    // canonicalize, always check movement from left piece
    what = '[';
    fromx--;
  }

  int tox, toy;
  fwd(fromx, fromy, dir, &tox, &toy);
  char at = grid_at(g, tox, toy);

  //printf("can move? %c(%d,%d) => %c(%d,%d) in dir %d\n", what, fromx, fromy, at,tox, toy, dir);

  bool box = what=='[';
  char at1 = grid_at(g, tox+1, toy); // right side candidate pos
  // can't move into walls
  if(at == '#') return false;
  if(box && at1=='#') return false;
  if(at == '.' && (!box || at1 == '.')) return true; // simple case of moving to free space
  // if left/right try to find 1 space before hitting a wall
  if(dir == 1 || dir == 3) {
    int dx = dir==1 ? 1 : -1;
    int tx = fromx + dx;
    while(grid_at(g, tx, fromy)!='#') {
      if(grid_at(g, tx, fromy) == '.') return true;
      tx += dx;
    }
    return false;
  } else {
    // move up/down
    if(!can_move(g, at, tox, toy, dir)) {
      //printf("%c(%d,%d) left side can't move\n", at,tox,toy);
      return false;
    }
    if(box && at1 != '.' && !can_move(g, at1, tox+1, toy, dir)) {
      //printf("%c(%d,%d) right side can't move\n", at1,tox+1,toy);
      return false;
    }
  }
  // nothing
  return true;
}

// boxes are now [], this assumes can move is checked before calling
void move2(struct Grid *g, char what, int fromx, int fromy, int dir) {
  if(what == ']') {
    what = '[';
    fromx--;
  }
  bool box = what=='[';
  int tox, toy;
  fwd(fromx, fromy, dir, &tox, &toy);
  char at = grid_at(g, tox, toy);

  //printf("  move %c(%d,%d) => %c(%d,%d)\n", what, fromx, fromy, at, tox,toy);

  // clear my position
  grid_set(g, fromx, fromy, '.');
  if(box) grid_set(g, fromx+1, fromy, '.');

  at = grid_at(g, tox, toy);
  if(at == '[' || at == ']') {
    // move item in the way
    //printf(" %c(%d,%d) in the way\n", at, tox, toy);
    move2(g, at, tox, toy, dir);
  }
  if(box) {
    at = grid_at(g, tox+1,toy);
    if(at == '[' || at == ']') {
      //printf("  move other half!\n");
      move2(g, at, tox+1,toy,dir);
    }
  }


  grid_set(g, tox, toy, what);
  if(box) grid_set(g, tox+1, toy, ']');

}

int ch_to_dir(char ins)  {
  switch(ins) {
  case '^': return 0;
  case '>': return 1;
  case 'v': return 2;
  case '<': return 3;
  default: panic("No such direction: %c", ins);
  }
}

// move robot from current pos (cx,cy) to new pos (nx,ny) if possible
// modifies grid as necessary
void instruction(struct Grid *g, int cx, int cy, char ins, int *nx, int *ny) {
  if(ins == '\n') {
    *nx = cx;
    *ny = cy;
    return;
  }
  int dir = ch_to_dir(ins);
  // check recursively if we can move in dir
  if(move(g, '@', cx, cy, dir)) {
    fwd(cx, cy, dir, nx, ny);
  } else {
    *nx = cx;
    *ny = cy;
  }
}

void instruction2(struct Grid *g, int cx, int cy, char ins, int *nx, int *ny) {
  int dir = ch_to_dir(ins);
  if(can_move(g, '@', cx, cy, dir)) {
    //printf("MOVING\n");
    move2(g, '@', cx, cy, dir);
    fwd(cx, cy, dir, nx, ny);
  } else {
    //printf("CAN'T MOVE\n");
    *nx = cx;
    *ny = cy;
  }
  //printf("DONE\n");
}


struct Grid *expand(struct Grid *in) {
  struct Grid *out = grid_new(in->w*2, in->h);
  grid_each(in, x, y, val, {
      char c1;
      char c2;
      switch(val) {
      case '#': c1 = '#'; c2 = '#'; break;
      case '@': c1 = '@'; c2 = '.'; break;
      case 'O': c1 = '['; c2 = ']'; break;
      case '.': c1 = '.'; c2 = '.'; break;
      }
      grid_set(out, x*2+0, y, c1);
      grid_set(out, x*2+1, y, c2);
    });
  return out;
}

void start_pos(struct Grid *g, int *x, int *y) { grid_find(g, '@', *x, *y); }

aoc_main({
    size_t sz;
    char *in = input("day15.txt", &sz);

    InitWindow(1200, 900, "Warehouse mayhem");
    SetTargetFPS(240);

    // input changes from frid to moves when first double newline is encountered
    char *split = strstr(in, "\n\n");
    split[1] = 0;
    char *inst = split + 2;

    struct Grid *g = grid_from_string(in);
    struct Grid *g2 = expand(g);
    int x;
    int y;
    start_pos(g, &x, &y);

    while(*inst != 0) {
      instruction(g, x, y, *inst, &x, &y);
      inst++;
    }
    int gps=0;
    grid_each(g,x,y,val, {
        if(val=='O') {
          gps += 100*y+x;
        }
      });

    printf("Part1: %d\n", gps);


    inst = split + 2;
    start_pos(g2, &x, &y);
    int last_dir = 0;
    bool game = false;
    bool draw_on = false;
    while(!WindowShouldClose() && *inst != 0) {
      if(game) {
        if(IsKeyPressed(KEY_UP)) { instruction2(g2, x, y, '^', &x, &y); last_dir = 0; }
        if(IsKeyPressed(KEY_RIGHT)) { instruction2(g2, x, y, '>', &x, &y); last_dir = 1; }
        if(IsKeyPressed(KEY_DOWN)) { instruction2(g2, x, y, 'v', &x, &y); last_dir = 2; }
        if(IsKeyPressed(KEY_LEFT)) { instruction2(g2, x, y, '<', &x, &y); last_dir = 3; }
        draw(g2, last_dir);
      } else {
        if(*inst == '\n') { inst++; continue; }
         if(*inst == 0)
           draw(g2, 0);
         else {
           if(draw_on) draw(g2, ch_to_dir(*inst));
           instruction2(g2, x, y, *inst, &x, &y);
           inst++;
         }
      }
    }
    gps = 0;
    grid_each(g2,x,y,val, {
        if(val=='[') {
          gps += 100*y+x;
        }
      });
    printf("Part2: %d\n", gps);



  });
