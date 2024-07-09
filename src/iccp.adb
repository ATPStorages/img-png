with PLTE;
with IDAT;
with sRGB;

package body iCCP is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Offset : Natural := 0;
   begin
      if PNG.Chunk_Count (V, PLTE.TypeRaw) > 0 then
         declare
            Structure_Error : PNG.Decoder_Error (PNG.BAD_ORDER);
         begin
            Structure_Error.Constraints.Insert (PLTE.TypeRaw, PNG.AFTER);
            C.Data.Errors.Append (Structure_Error);
         end;
      end if;

      if PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         declare
            Structure_Error : PNG.Decoder_Error (PNG.BAD_ORDER);
         begin
            Structure_Error.Constraints.Insert (IDAT.TypeRaw, PNG.AFTER);
            C.Data.Errors.Append (Structure_Error);
         end;
      end if;

      if PNG.Chunk_Count (V, sRGB.TypeRaw) > 0 then
         declare
            Exclusive_Error : PNG.Decoder_Error (PNG.MUTUALLY_EXCLUSIVE);
         begin
            Exclusive_Error.To := sRGB.TypeRaw;
            C.Data.Errors.Append (Exclusive_Error);
         end;
      end if;

      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         declare
            Duplicate_Error : PNG.Decoder_Error (PNG.DUPLICATE_CHUNK);
         begin
            C.Data.Errors.Append (Duplicate_Error);
         end;
      end if;

      Self.ProfileName := PNG.Decode_Null_String (S, Offset);
      PNG.Compression_Method'Read (S, Self.CompressionMethod);
      Self.Profile := To_Unbounded_String (PNG.Decode_String_Chunk_End
                                           (S, F, C));
   end Decode;

end iCCP;
