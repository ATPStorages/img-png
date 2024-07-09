with IDAT;

package body PLTE is

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

      if Chunk_Count (V, IDAT.TypeRaw) > 0 then
         if PNG.Chunk_Count (V, TypeRaw) > 0 then
            declare
               Structure_Error : PNG.Decoder_Error (PNG.BAD_ORDER);
            begin
               Structure_Error.Constraints.Insert (IDAT.TypeRaw, AFTER);
               C.Data.Errors.Append (Structure_Error);
            end;
         end if;
      end if;

      Data_Definition'Read (S, Self);
   end Decode;

end PLTE;
