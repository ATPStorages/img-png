with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Interfaces; use Interfaces;

with PNG;
with Compression.ZLib;

package IDAT is

   TypeRaw : constant PNG.Chunk_Type := 16#49444154#;

   type Data_Definition (CompressedDataLength : Unsigned_32)
   is new PNG.Chunk_Data_Definition with record
      Data : Compression.ZLib.Data (CompressedDataLength);
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end IDAT;
