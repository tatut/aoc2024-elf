#include "aoc.h"
#include <string.h>
#include <unistd.h>

struct Robot {
  int x, y, vx, vy;
};

void parse(struct Robot *r, char *line) {
  char *at = line + 2; // skip "p="
  r->x = number_neg(at, &at);
  at++; // skip ","
  r->y = number_neg(at, &at);
  at += 3; // skip " v="
  r->vx = number_neg(at, &at);
  at++; // skip ","
  r->vy = number_neg(at, &at);
}

#define W 101
#define H 103

void move(struct Robot *r) {
  int nx = r->x + r->vx,
    ny = r->y + r->vy;
  if(nx < 0) nx = W + nx;
  else nx %= W;
  if(ny < 0) ny = H + ny;
  else ny %= H;
  r->x = nx;
  r->y = ny;
}

void quadrant(struct Robot *r, size_t *q1, size_t *q2, size_t *q3, size_t *q4) {
  if(r->x < 50 && r->y < 51) *q1 = *q1 + 1;
  if(r->x > 50 && r->y < 51) *q2 = *q2 + 1;
  if(r->x < 50 && r->y > 51) *q3 = *q3 + 1;
  if(r->x > 50 && r->y > 51) *q4 = *q4 + 1;
}

void move_all(struct Robot *r, size_t robots) {
  for(size_t i=0;i<robots;i++) move(&r[i]);
}

long part1(struct Robot *r, size_t robots) {
  int i,j;
  for(i=0;i<100;i++) {
    move_all(r, robots);
  }
  size_t q1=0,q2=0,q3=0,q4=0;
  for(j=0;j<robots;j++)
    quadrant(&r[j], &q1, &q2, &q3, &q4);
  return q1*q2*q3*q4;
}

// area sized string with '\n' line ends and nul terminator
char area[((W + 1) * H) + 1];

void show(struct Robot *r, size_t robots) {
  size_t i,j;
  memset(&area[0], '.', ((W + 1) * H));
  area[((W + 1) * H)] = 0;
  for(i=0;i<=H;i++) { area[i*(W+1)+W] = '\n'; }

  for(j=0;j<robots;j++)
    area[ r[j].y * (W+1) + r[j].x ] = '#';

  //printf("%s", &area[0]);
}

const char *tree[] = {
  "###############################",
  "#.............................#",
  "#.............................#",
  "#.............................#",
  "#.............................#",
  "#..............#..............#",
  "#.............###.............#",
  "#............#####............#",
  "#...........#######...........#",
  "#..........#########..........#",
  "#............#####............#",
  "#...........#######...........#",
  "#..........#########..........#",
  "#.........###########.........#",
  "#........#############........#",
  "#..........#########..........#",
  "#.........###########.........#",
  "#........#############........#",
  "#.......###############.......#",
  "#......#################......#",
  "#........#############........#",
  "#.......###############.......#",
  "#......#################......#",
  "#.....###################.....#",
  "#....#####################....#",
  "#.............###.............#",
  "#.............###.............#",
  "#.............###.............#",
  "#.............................#",
  "#.............................#",
  "#.............................#",
  "#.............................#",
  "###############################" };

bool tree_found() {
  char *at = area;
  for(int line=0;line<33; line++) {
    char *found = strstr(at, tree[line]);
    if(!found) return false;
    at = found + W +1; // go down 1 line
  }
  return true;
}

aoc_main({
    size_t sz;
    size_t robots;
    char *in = input("day14.txt", &sz);
    char ch;
    robots = lines_count(in);

    struct Robot *r = malloc(robots*sizeof(struct Robot));
    lines_each(in, i, line, { parse(&r[i], line); });

    printf("Part1: %ld\n", part1(r, robots)); // 223020000

    size_t rounds=100;
    show(r, robots);
    while(!tree_found()) {
      move_all(r,robots);
      rounds++;
      show(r, robots);
    }
    printf("Part2: %ld\n", rounds); // 7338
  });
