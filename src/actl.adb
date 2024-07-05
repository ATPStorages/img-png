with PLTE;
with IDAT;

package body acTL is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "Only 1 acTL chunk is permitted in a PNG datastream";
      elsif
        PNG.Chunk_Count (V, PLTE.TypeRaw) > 0 or else
        PNG.Chunk_Count (V, IDAT.TypeRaw) > 0
      then
         raise PNG.BAD_STRUCTURE_ERROR
         with "acTL must come before PLTE/IDAT";
      end if;

      Data_Definition'Read (S, Self);

      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FrameCount);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.RepeatCount);
   end Decode;

end acTL;
