with PLTE;
with IDAT;

package body cICP is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR with
           "A valid PNG stream must contain only 1 cICP chunk";
      elsif PNG.Chunk_Count (V, PLTE.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR with
           "The cICP chunk must come before the PLTE chunk";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR with
           "The cICP chunk must chome before the first IDAT chunk";
      end if;

      Data_Definition'Read (S, Self);
   end Decode;

end cICP;
