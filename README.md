# gfxlang-hs
gfxlang-hs is a transpiler written in Haskell that produces C code that makes use of Tsoding's Olivec library

## Building/Installing
```sh
$ git clone https://github.com/rfmineguy/gfx-lang.git
$ stack install 
```

## Dependencies
This project relies on the presence of Olivec and stbimagewrite
These should be in the same folder as your .gfx file
```sh
$ wget https://raw.githubusercontent.com/tsoding/olive.c/master/olive.c
$ wget https://raw.githubusercontent.com/nothings/stb/master/stb_image_write.h
```

## Usage
I only support producing a png file at the moment (and probably forever) so keep that in mind
```sh
$ gfxlang-hs-exe -f a.gfx -o a.c
$ gcc a.c -o a
$ ./a
```

## Examples
```
settings () {
  outFile = "image.png"
  width = 600
  height = 600
}
run () {
  fill(0xFF454545)
  circle(300 300 180 0xFF2D00BC)
  circle(300 300 90 0xFF459832)
  rect(30 30 50 50 0xFF994213)
  line(20 40 400 300 0xFF532156)
}
```

## Explaination
- settings () is a required function where you specify all of the varaibles you will use
    - Currently the only variables used here are required
        - outFile -> Specifies the output filename (will always be png format)
        - width   -> Specifies the width of the output image
        - height  -> Specifies the height of the output image
- run () is another required function you put all the code that draws stuff
    - The functions callable in here are the same as those present in the normal OliveC library


