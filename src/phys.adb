with Interfaces; use Interfaces;

with IDAT;

package body pHYs is

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
         with "There may only be one pHYs chunk";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "pHYs must come before the first IDAT chunk";
      end if;

      -- See ihdr.adb

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.PixelsPerHorizontalUnit := PNG.Unsigned_31 (Unsigned_32_Buffer);

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.PixelsPerVerticalUnit := PNG.Unsigned_31 (Unsigned_32_Buffer);

      Unit_Type'Read (S, Self.Unit);
   end Decode;

end pHYs;
