#include "aoc.h"
#include "raylib.h"
#include "raymath.h"
#include "colors.h"
#include <math.h>
#include <string.h>
#include <unistd.h>

// size of grid cell in pixels
#define PX 32

const char *dirs = "^>v<";

Camera2D camera = {{600,450}, {100,100}, 0, 1};
float rounds = 0;
Texture2D tileset;

void draw(struct Grid *g, int dir, int x, int y) {
  BeginDrawing();
  ClearBackground(BLACK);
  BeginMode2D(camera);

  camera.target.x = Lerp(camera.target.x, (float) (x*PX), 0.03);
  camera.target.y = Lerp(camera.target.y, (float) (y*PX), 0.03);
  camera.zoom = 1.0f;
  camera.rotation = 2. * sin(rounds);
  rounds+= 0.03;

  grid_each(g, x, y, val, {
      int cx = x*PX+PX/2;
      int cy = y*PX+PX/2;
      DrawTextureRec(tileset, (Rectangle) { 64,0, 32, 32 }, (Vector2) { (float) (x*PX), (float) (y*PX) }, WHITE);
      switch(val) {
      case '@':
        DrawTextureRec(tileset, (Rectangle) { (float) (96+dir*32),0, 32, 32 }, (Vector2) { (float) (x*PX), (float) (y*PX) }, WHITE);
        break;
      case '#':
        DrawTextureRec(tileset, (Rectangle) { 0,0, 32, 32 }, (Vector2) { (float) (x*PX), (float) (y*PX) }, WHITE);
        break;
      case 'O':
        DrawTextureRec(tileset, (Rectangle) { 32, 0, 32, 32 }, (Vector2) { (float) (x*PX), (float) (y*PX) }, WHITE);
        break;
      case '[':
        DrawTextureRec(tileset, (Rectangle) { 224, 0, 32, 32 }, (Vector2) { (float) (x*PX), (float) (y*PX) }, WHITE);
        break;
      case ']':
        DrawTextureRec(tileset, (Rectangle) { 256, 0, 32, 32 }, (Vector2) { (float) (x*PX), (float) (y*PX) }, WHITE);
        break;
      }
    });
  EndMode2D();
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

bool is_box(char ch) { return ch == 'O' || ch == '[' || ch == ']'; }

// move what into x,y position, shifting any items into dir
// returns true if we  moved, false if we failed
// if dry_run is true, the grid isn't modified, only check if we could
bool move(struct Grid *g, int fromx, int fromy, int dir, bool dry_run) {
  char what = grid_at(g, fromx, fromy);
  //printf("move(%c,%d,%d,%d,%d)\n", what,fromx,fromy,dir,dry_run);
  if(what=='#') return false; // can't move walls
  if(what=='.') return true; // don't need to move empty space
  if(what==']') { // consider only the left side of big box
    what='[';
    fromx--;
  }
  bool big_box_l = what=='[';
  int tox, toy;
  fwd(fromx, fromy, dir, &tox, &toy);

  if(dir == 3) {
    // move left
    if(!move(g, fromx-1, fromy, dir, dry_run))
      return false;
  } else if(dir == 1) {
    // move right
    int ex = fromx + (big_box_l?2:1);
    if(!move(g, ex, fromy, dir, dry_run))
      return false;
  } else {

    if(!move(g, tox, toy, dir, dry_run))
      return false;
    // if [] box, check other side can move as well
    if(big_box_l) {
      if(!move(g, tox+1,toy,dir,dry_run))
        return false;
    }
  }

  if(!dry_run) {
    // actually move
    // no problem to moving
    grid_set(g, fromx, fromy, '.');
    if(big_box_l) grid_set(g, fromx+1, fromy, '.');
    grid_set(g, tox, toy, what);
    if(big_box_l) grid_set(g, tox+1, toy, ']');
  }
  return true;
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
  if(move(g, cx, cy, dir, true)) {
    move(g,cx,cy,dir,false);
    fwd(cx, cy, dir, nx, ny);
  } else {
    *nx = cx;
    *ny = cy;
  }
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

void play(struct Grid *g, int x, int y) {
  int last_dir=0;
  while(!WindowShouldClose()) {
    if(IsKeyPressed(KEY_UP)) { instruction(g, x, y, '^', &x, &y); last_dir = 0; }
    if(IsKeyPressed(KEY_RIGHT)) { instruction(g, x, y, '>', &x, &y); last_dir = 1; }
    if(IsKeyPressed(KEY_DOWN)) { instruction(g, x, y, 'v', &x, &y); last_dir = 2; }
    if(IsKeyPressed(KEY_LEFT)) { instruction(g, x, y, '<', &x, &y); last_dir = 3; }
    draw(g, last_dir, x ,y);
  }
}

aoc_main({
    size_t sz;
    char *in = input("day15.txt", &sz);

    InitWindow(1200, 900, "Varastomähinä!");
    SetTargetFPS(60);
    tileset = LoadTexture("tileset.png");

    // input changes from frid to moves when first double newline is encountered
    char *split = strstr(in, "\n\n");
    split[1] = 0;
    char *inst = split + 2;

    struct Grid *g = grid_from_string(in);
    struct Grid *g2 = expand(g);
    int x;
    int y;
    bool draw_on = false;
    start_pos(g, &x, &y);
    //play(g, x, y);
    while(*inst != 0) {
      //if(*inst != '\n' && draw_on) draw(g, ch_to_dir(*inst), x, y);
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
    while(*inst != 0) {
      if(*inst != '\n' && draw_on) draw(g2, ch_to_dir(*inst), x, y);
      instruction(g2, x, y, *inst, &x, &y);
      inst++;
    }
    gps = 0;
    grid_each(g2,x,y,val, {
        if(val=='[') {
          gps += 100*y+x;
        }
      });
    printf("Part2: %d\n", gps);



  });
