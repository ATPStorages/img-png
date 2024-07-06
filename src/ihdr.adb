with Ada.Containers; use Ada.Containers;
with PNG; use PNG;

package body IHDR is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : Ada.Streams.Stream_IO.File_Type)
   is
   begin
      if V.Length > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "Only 1 IHDR chunk can be in a PNG datastream";
      end if;

      Data_Definition'Read (S, Self);

      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.Width);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.Height);
   end Decode;

end IHDR;
