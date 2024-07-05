with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package IDAT is

   TypeRaw : constant PNG.Chunk_Type := 16#49444154#;

   type Data_Definition (DataLength : PNG.Unsigned_31) is
     new PNG.Base_Chunk_Data_Definition with record
      FrameData : PNG.Chunk_Data_Array (1 .. DataLength);
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end IDAT;
