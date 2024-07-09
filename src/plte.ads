with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG; use PNG;

package PLTE is

   TypeRaw : constant PNG.Chunk_Type := 16#504C5445#;

   subtype Palette_Length is Unsigned_31 range 1 .. 256;
   type Palette_Color_Data is array (1 .. 3) of Unsigned_8;
   type Palette_Data is
     array (Palette_Length range <>)
     of Palette_Color_Data;
   subtype Palette_Data_Length
     is Unsigned_31
       range Palette_Length'First * 3 .. Palette_Length'Last * 3
     with Dynamic_Predicate => Palette_Data_Length mod 3 = 0;

   type Data_Definition
     (DataLength    : Palette_Data_Length;
      PaletteLength : Palette_Length)
   is new PNG.Chunk_Data_Definition with record
      Palette : Palette_Data (1 .. PaletteLength);
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end PLTE;
