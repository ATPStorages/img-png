with Interfaces; use Interfaces;

with PLTE;
with IDAT;

package body acTL is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Unsigned_32_Buffer : Unsigned_32;
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

      -- See ihdr.adb

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FrameCount := PNG.Unsigned_31 (Unsigned_32_Buffer);

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.RepeatCount := PNG.Unsigned_31 (Unsigned_32_Buffer);
   end Decode;

end acTL;
