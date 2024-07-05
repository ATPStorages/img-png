with IDAT;

package body pHYs is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "There may only be one pHYs chunk";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "pHYs must come before the first IDAT chunk";
      end if;

      Data_Definition'Read (S, Self);

      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.PixelsPerHorizontalUnit);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.PixelsPerVerticalUnit);
   end Decode;

end pHYs;
