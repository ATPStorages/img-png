with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;

with PNG;

package cICP is
   
   TypeRaw : constant PNG.Chunk_Type := 16#63494350#;

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      ColorPrimaries     : Unsigned_8; 
      TransferFunction   : Unsigned_8; 
      MatrixCoefficients : Unsigned_8 range 0 .. 0; 
      VideoFullRangeFlag : Boolean; 
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end cICP;
