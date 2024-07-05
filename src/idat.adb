with Interfaces; use Interfaces;

package body IDAT is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if
        V.Last_Element.TypeInfo.Raw /= TypeRaw and then
        PNG.Chunk_Count (V, TypeRaw) > 0
      then
         raise PNG.BAD_STRUCTURE_ERROR
           with "IDAT chunks should be consective in a valid PNG datastream";
      end if;

      Data_Definition'Read (S, Self);
   end Decode;

end IDAT;
