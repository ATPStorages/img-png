with PLTE;
with IDAT;
with sRGB;

package body iCCP is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Offset : Natural := 0;
   begin
      if PNG.Chunk_Count (V, PLTE.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The iCCP chunk should appear before the PLTE chunk";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The iCCP chunk should appear before the first IDAT chunk";
      elsif PNG.Chunk_Count (V, sRGB.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The iCCP chunk should not be present with an sRGB chunk";
      elsif PNG.Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "There must be only one iCCP chunk";
      end if;

      Self.ProfileName := PNG.Decode_Null_String (S, Offset);
      PNG.Compression_Method'Read (S, Self.CompressionMethod);
      Self.Profile := To_Unbounded_String (PNG.Decode_String_Chunk_End (S,
                                           Natural (C.Length),
                                           Offset));
   end Decode;

end iCCP;
