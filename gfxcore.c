#include <raylib.h>

int window_width = 600, window_height = 600;

void size(int w, int h) {
  window_width = w;
  window_height = h;
}

void framerate(int fr) {
  SetTargetFPS(fr);
}

void settings();
void begin();
void draw(float dt);

int main() {
  settings();
  InitWindow(window_width, window_height, "Window");
}
