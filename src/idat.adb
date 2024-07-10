with Interfaces; use Interfaces;

package body IDAT is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if
        V.Last_Element.TypeInfo.Raw /= TypeRaw and then
        PNG.Chunk_Count (V, TypeRaw) > 0
      then
         declare
            Structure_Error : PNG.Decoder_Error (PNG.BAD_ORDER);
         begin
            Structure_Error.Constraints.Insert (TypeRaw, PNG.BEFORE);
            C.Data.Errors.Append (Structure_Error);
         end;
      end if;

      -- Data_Definition'Read (S, Self);
   end Decode;

end IDAT;
