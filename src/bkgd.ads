with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package bKGD is

   TypeRaw : constant PNG.Chunk_Type := 16#624B4744#;

   type Color_Definition_Access is access PNG.Chunk_Data_Definition'Class;

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      Color_Definition : Color_Definition_Access;
   end record;

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Data_Definition);

   for Data_Definition'Read use Read;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

   type Grayscale_Definition is new Data_Definition with record
      GrayLevel : Unsigned_16;
   end record;

   type Grayscale_Definition_Access is access Grayscale_Definition;

   type Truecolor_Definition is new Data_Definition with record
      R : Unsigned_16;
      G : Unsigned_16;
      B : Unsigned_16;
   end record;

   type Truecolor_Definition_Access is access Truecolor_Definition;

   type Palette_Definition is new Data_Definition with record
      Index : Unsigned_8;
   end record;

   type Palette_Definition_Access is access Palette_Definition;

end bKGD;
