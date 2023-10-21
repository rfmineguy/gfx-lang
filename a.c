void size(int, int);
void settings();
void begin();
void draw(float dt);
int angle;
int someVar;
int x;
int y;
void settings(){
	size(600,600);
	framerate(30);
}
void begin(){
	x = 4;
	y = 6;
	angle = 0;
}
void draw(int dt){
	angle = 3;
	someVar = 4;
	circle(x,y,10);
}
#include "gfxcore.c"
