with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package IHDR is

   TypeRaw : constant PNG.Chunk_Type := 16#49484452#;

   type ColorTypes is (GRAYSCALE,
                       TRUECOLOR,
                       INDEXED_COLOR,
                       GRAYSCALE_WITH_ALPHA,
                       TRUECOLOR_WITH_ALPHA)
     with Size => 8;

   for ColorTypes use (GRAYSCALE => 0,
                       TRUECOLOR => 2,
                       INDEXED_COLOR => 3,
                       GRAYSCALE_WITH_ALPHA => 4,
                       TRUECOLOR_WITH_ALPHA => 6);

   type Data_Definition is new PNG.Base_Chunk_Data_Definition with record
      Width             : PNG.Unsigned_31_Positive;
      --  Width of the image.
      Height            : PNG.Unsigned_31_Positive;
      --  Height of the image.
      BitDepth          : Unsigned_8;
      --  Bit depth of the pixels within this image.
      ColorType         : ColorTypes;
      CompressionMethod : Unsigned_8 range 0 .. 0;
      FilterMethod      : Unsigned_8 range 0 .. 0;
      InterlaceMethod   : Unsigned_8 range 0 .. 1;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : Ada.Streams.Stream_IO.File_Type);

end IHDR;
