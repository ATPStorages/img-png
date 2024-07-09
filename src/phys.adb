with Interfaces; use Interfaces;

with IDAT;

package body pHYs is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Unsigned_32_Buffer : Unsigned_32;
   begin
      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         declare
            Duplicate_Error : PNG.Decoder_Error (PNG.DUPLICATE_CHUNK);
         begin
            C.Data.Errors.Append (Duplicate_Error);
         end;
      end if;

      if PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         if PNG.Chunk_Count (V, TypeRaw) > 0 then
            declare
               Structure_Error : PNG.Decoder_Error (PNG.BAD_ORDER);
            begin
               Structure_Error.Constraints.Insert (IDAT.TypeRaw, PNG.AFTER);
               C.Data.Errors.Append (Structure_Error);
            end;
         end if;
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
