with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package iCCP is

   TypeRaw : constant PNG.Chunk_Type := 16#69434350#;

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      ProfileName       : Unbounded_String;
      CompressionMethod : PNG.Compression_Method;
      Profile           : Unbounded_String;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end iCCP;
