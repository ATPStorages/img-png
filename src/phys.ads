with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package pHYs is

   TypeRaw : constant PNG.Chunk_Type := 16#70485973#;

   type Unit_Type is (UNKNOWN, METER)
     with Size => 8;

   for Unit_Type use (UNKNOWN => 0,
                      METER => 1);

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      PixelsPerHorizontalUnit : PNG.Unsigned_31;
      PixelsPerVerticalUnit   : PNG.Unsigned_31;
      Unit                    : Unit_Type;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end pHYs;
