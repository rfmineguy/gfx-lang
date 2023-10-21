#define OLIVEC_IMPLEMENTATION
#include "olive.c"
Olivec_Canvas oc;
const char* outFile;
int height;
int width;
void settings(){
	outFile = "image.png";
	width = 600;
	height = 600;
}
void run(){
	olivec_fill(oc,42,53,255,43);
	olivec_circle(oc,4,5,180,4281139388);
}
int main(void) {
	settings();
	oc = olivc_canvas(pixels, width, height, width);
	if (!stbi_write_png(outFile, width, height, 4, pixels, sizeof(uint32_t)*width)) { return 1; }
	return 0;
 }