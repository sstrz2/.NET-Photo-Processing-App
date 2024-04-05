//
// F# image processing functions.
//
// This program works in conjuction with a frontend program to provide a user with options to alter an image
//
// Seth Strzechowski
// UIC CS 341 Spring 2024
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // Sepia:
  //
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //

    let rec SepiaRow row = 
        match row with
        | [] -> []
        | (r,g,b)::tail ->
            // Calculate new RGB values based on sepia transformation formula
            let red = 0.393 * float r + 0.769 * float g + 0.189 * float b
            let green = 0.349 * float r + 0.686 * float g + 0.168 * float b
            let blue = 0.272 * float r + 0.534 * float g + 0.131 * float b

            // Clamp calculated values to the range [0, 255]
            let newRed =
                if red > 255.0 then 255 else int red
            let newGreen =
                if green > 255.0 then 255 else int green
            let newBlue =
                if blue > 255.0 then 255 else int blue

            // Recursively apply SepiaRow function to the remaining tail of the row
            // and prepend the modified pixel to the result
            (newRed,newGreen,newBlue)::SepiaRow tail
            
    let rec Sepia (width:int) (height:int) (depth:int) (image:(int*int*int) list list) =
        match image with
        | [] -> []
        | head::tail ->
            SepiaRow head::Sepia width height depth tail

  //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected should be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //
    let rec IntensityRow row factor color=
        match row with
        | [] -> []
        | (r,g,b)::tail ->
             // Calculate new intensity values for the specified color channel
            let red =
                if color = 'r' then int(double r*factor) else r
            let green =
                if color = 'g' then int(double g*factor) else g
            let blue =
                if color = 'b' then int(double b*factor) else b
            
            // Clamp intensity values to the range [0, 255]
            let newRed =
                if red > 255 then 255 else red
            let newGreen =
                if green > 255 then 255 else green
            let newBlue =
                if blue > 255 then 255 else blue
                
            (newRed,newGreen,newBlue)::IntensityRow tail factor color
    
    //calls itself and prepends IntensityRow to the final return for the current row
    let rec IncreaseIntensity (width:int) (height:int)(depth:int)(image:(int*int*int) list list)(intensity:double)(channel:char) = 
        match image with
        | [] -> []
        | head::tail ->
            IntensityRow head intensity channel::IncreaseIntensity width height depth tail intensity channel


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
    let rec FlipHorizontal (width:int)(height:int)(depth:int)(image:(int*int*int) list list) = 
        //I simply use the List.rev to reverse the order of each row of pixels
        List.map List.rev image

  //
  // Rotate180:
  //
  // Rotates the image 180 degrees.
  //
  // Returns: updated image.
  //
    let rec Rotate180 (width:int)(height:int)(depth:int)(image:(int*int*int) list list) = 
        //A 180 degree rotation is just a List.rev action twice
        List.rev (List.map List.rev image)


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
    let dist (r1, g1, b1) (r2, g2, b2) =
        let square x = x * x
        sqrt (square (float (r1 - r2)) + square (float (g1 - g2)) + square (float (b1 - b2)))

    let rec EdgeRow row next limit=
        match row,next with
        | [],[] -> []
        | (rc,gc,bc)::tail1,(rb,gb,bb)::tail2 ->
            match tail1 with
            | [] -> []
            | (rr,gr,br)::tail3 ->
                //if the dist is greater than the limit, the pixel is black (0,0,0)
                if dist (rc,gc,bc) (rb,gb,bb) > limit then
                    (0,0,0)::EdgeRow tail1 tail2 limit
                elif dist (rc,gc,bc) (rr,gr,br) > limit then
                    (0,0,0)::EdgeRow tail1 tail2 limit
                else //otherwise the pixel is white (255,255,255)
                    (255,255,255)::EdgeRow tail1 tail2 limit

    let rec EdgeDetect (width:int)(height:int)(depth:int)(image:(int*int*int) list list)(threshold:int) = 
        match image with
        | [] -> []
        | head1::tail1 ->
            match tail1 with
            | [] -> []
            | head2::tail2 ->
                EdgeRow head1 head2 threshold::EdgeDetect width height depth tail1 threshold

