with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package acTL is

   TypeRaw : constant PNG.Chunk_Type := 16#6163544C#;

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      FrameCount  : PNG.Unsigned_31;
      RepeatCount : PNG.Unsigned_31;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end acTL;
