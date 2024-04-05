//
// Main program for image processing functions. This 
// program inputs the PPM image file, calls the appropriate
// F# image function, and writes the resulting PPM file.
// To view the resulting PPM files, you'll need some sort
// of image viewing program, e.g. ppmReader.html
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
// Modified: Ellen Kidane, UIC, Spring 2024
//

using System;
using System.IO;
using Microsoft.FSharp.Collections;

namespace ImageMain
{
   class Program
   {

      static void Main(string[] args)
      {
        int width, height, depth;
        FSharpList<FSharpList<Tuple<int,int,int>>> pixels;

        //
        // Input a PPM image file:
        //
        Console.Write("Image filename> ");
        string filename = Console.ReadLine();
        
        bool success = Utility.ReadImageFile(filename, 
          out width, out height, out depth, out pixels);

        if (!success) {
          Console.WriteLine("**Error: unable to open '{0}'", filename);
          return;
        }

        //
        // What operation does the user want to perform?
        //
        Console.WriteLine();
        Console.WriteLine("Operation?");
        Console.WriteLine("  1 => sepia");
        Console.WriteLine("  2 => increase intensity of channel");
        Console.WriteLine("  3 => flip horizontal");
        Console.WriteLine("  4 => rotate 180 degrees");
        Console.WriteLine("  5 => edge detect");
        Console.Write("> ");
        int cmd = Convert.ToInt32(Console.ReadLine());
        Console.WriteLine();

        //
        // Operations:
        //
        if (cmd == 1) {
          Console.WriteLine("Converting to sepia...");
          FSharpList<FSharpList<Tuple<int, int, int>>> newPixels;
          newPixels = ImageLibrary.Operations.Sepia(width, height, depth, pixels);

          string outfile = System.IO.Path.GetFileNameWithoutExtension(filename);
          outfile = outfile + "-sepia.ppm";
          Console.WriteLine("Writing '{0}'...", outfile);
          Utility.WriteImageFile(outfile, depth, ref newPixels);
        }
        else if (cmd == 2) {
          Console.Write("Intensity value? ");
          double intensity = Convert.ToDouble(Console.ReadLine());

          Console.Write("Channel (r for red, g for green, or b for blue)? ");
          char channel = Convert.ToChar(Console.ReadLine());

          Console.WriteLine("Increasing intensity...");
          FSharpList<FSharpList<Tuple<int, int, int>>> newPixels;
          newPixels = ImageLibrary.Operations.IncreaseIntensity(width, height, depth, pixels, intensity, channel);

          string outfile = System.IO.Path.GetFileNameWithoutExtension(filename);
          outfile = outfile + "-intensified.ppm";
          Console.WriteLine("Writing '{0}'...", outfile);
          Utility.WriteImageFile(outfile, depth, ref newPixels);
        }
        else if (cmd == 3) {
          Console.WriteLine("flipping horizontally...");
          FSharpList<FSharpList<Tuple<int, int, int>>> newPixels;
          newPixels = ImageLibrary.Operations.FlipHorizontal(width, height, depth, pixels);

          string outfile = System.IO.Path.GetFileNameWithoutExtension(filename);
          outfile = outfile + "-flip-horz.ppm";
          Console.WriteLine("Writing '{0}'...", outfile);
          Utility.WriteImageFile(outfile, depth, ref newPixels);
        }
        else if (cmd == 4) {
          Console.WriteLine("rotating image 180 degrees...");
          FSharpList<FSharpList<Tuple<int, int, int>>> newPixels;
          newPixels = ImageLibrary.Operations.Rotate180(width, height, depth, pixels);

          string outfile = System.IO.Path.GetFileNameWithoutExtension(filename);
          outfile = outfile + "-rotated.ppm";
          Console.WriteLine("Writing '{0}'...", outfile);
          Utility.WriteImageFile(outfile, depth, ref newPixels);
        }
        else if (cmd == 5) {
          Console.Write("Edge threshold value (0 < value < 255)? ");
          int factor = Convert.ToInt32(Console.ReadLine());

          Console.WriteLine("Running edge detection...");
          FSharpList<FSharpList<Tuple<int, int, int>>> newPixels;
          newPixels = ImageLibrary.Operations.EdgeDetect(width, height, depth, pixels, factor);

          string outfile = System.IO.Path.GetFileNameWithoutExtension(filename);
          outfile = outfile + "-edge.ppm";
          Console.WriteLine("Writing '{0}'...", outfile);
          Utility.WriteImageFile(outfile, depth, ref newPixels);
        }
        else {
          Console.WriteLine("**Error, unknown command, please run again...");
        }

        Console.WriteLine();
        Console.WriteLine("Done");
      }//Main

   }//class
}//namespace
