with IDAT;

package body PLTE is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The PLTE chunk must come before the first IDAT chunk";
      elsif Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "There may only be one PLTE chunk";
      end if;

      Data_Definition'Read (S, Self);
   end Decode;

end PLTE;
