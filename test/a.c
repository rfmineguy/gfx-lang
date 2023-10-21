#define OLIVEC_IMPLEMENTATION
#include "olive.c"
#include <stdlib.h>
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
Olivec_Canvas oc;
uint32_t *pixels;
const char* outFile;
int height;
int width;
void settings(){
	outFile = "image.png";
	width = 600;
	height = 600;
}
void run(){
	olivec_fill(oc,4282729797);
	olivec_circle(oc,300,300,180,4281139388);
	olivec_circle(oc,300,300,90,4282751026);
}
int main(void) {
	settings();
	pixels = malloc(width * height * sizeof(uint32_t));
	oc = olivec_canvas(pixels, width, height, width);
	run();
	if (!stbi_write_png(outFile, width, height, 4, pixels, sizeof(uint32_t)*width)) { return 1; }
	free(pixels);
	return 0;
 }