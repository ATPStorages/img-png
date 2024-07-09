package body tIME is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         declare
            Duplicate_Error : PNG.Decoder_Error (PNG.DUPLICATE_CHUNK);
         begin
            C.Data.Errors.Append (Duplicate_Error);
         end;
      end if;

      Data_Definition'Read (S, Self);
      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.Year);
   end Decode;

end tIME;
