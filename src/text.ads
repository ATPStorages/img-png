with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package tEXt is

   TypeRaw : constant PNG.Chunk_Type := 16#74455874#;

   type Data_Definition is new PNG.Base_Chunk_Data_Definition with record
      Keyword : Unbounded_String;
      Text    : Unbounded_String;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end tEXt;
