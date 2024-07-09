with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package fdAT is

   TypeRaw : constant PNG.Chunk_Type := 16#66644154#;

   type Data_Definition (DataLength : PNG.Unsigned_31)
   is new PNG.Chunk_Data_Definition with record
      FramePosition : PNG.Unsigned_31;
      FrameData     : PNG.Chunk_Data_Array (1 .. DataLength);
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end fdAT;
