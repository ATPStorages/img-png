with PLTE;
with IDAT;
with iCCP;

package body sRGB is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, PLTE.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The sRGB chunk should appear before the PLTE chunk";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The sRGB chunk should appear before the first IDAT chunk";
      elsif PNG.Chunk_Count (V, iCCP.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The sRGB chunk should not be present with an iCCP chunk";
      elsif PNG.Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "There must be only one sRGB chunk";
      end if;

      Data_Definition'Read (S, Self);
   end Decode;

end sRGB;
