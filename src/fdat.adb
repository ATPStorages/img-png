with Interfaces; use Interfaces;

with IDAT;

package body fdAT is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Unsigned_32_Buffer : Unsigned_32;
   begin
      if PNG.Chunk_Count (V, IDAT.TypeRaw) < 1 then
         declare
            Structure_Error : PNG.Decoder_Error (PNG.BAD_ORDER);
         begin
            Structure_Error.Constraints.Insert (IDAT.TypeRaw, PNG.BEFORE);
            C.Data.Errors.Append (Structure_Error);
         end;
      end if;

      -- See ihdr.adb

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FramePosition := PNG.Unsigned_31 (Unsigned_32_Buffer);

      PNG.Chunk_Data_Array'Read (S, Self.FrameData);
   end Decode;

end fdAT;
