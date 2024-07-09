with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package IHDR is

   TypeRaw : constant PNG.Chunk_Type := 16#49484452#;

   type Color_Type is (GRAYSCALE,
                       TRUECOLOR,
                       INDEXED_COLOR,
                       GRAYSCALE_WITH_ALPHA,
                       TRUECOLOR_WITH_ALPHA)
     with Size => 8;

   for Color_Type use (GRAYSCALE => 0,
                       TRUECOLOR => 2,
                       INDEXED_COLOR => 3,
                       GRAYSCALE_WITH_ALPHA => 4,
                       TRUECOLOR_WITH_ALPHA => 6);

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      Width             : PNG.Unsigned_31_Positive;
      --  Width of the image.
      Height            : PNG.Unsigned_31_Positive;
      --  Height of the image.
      BitDepth          : Unsigned_8;
      --  Bit depth of the pixels within this image.
      ColorType         : Color_Type;
      CompressionMethod : PNG.Compression_Method;
      FilterMethod      : Unsigned_8 range 0 .. 0;
      InterlaceMethod   : Unsigned_8 range 0 .. 1;
   end record;

   type Data_Definition_Access is access all Data_Definition;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end IHDR;
