package body PNG is
   function PNG_Chunk_Hash (Chunk : PNG_Chunk) return Hash_Type
   is
   begin
      return Hash_Type (Chunk.CRC32);
   end PNG_Chunk_Hash;

   function PNG_Chunk_Equal_Element (A, B : PNG_Chunk) return Boolean
   is
   begin
      return A.CRC32 = B.CRC32;
   end PNG_Chunk_Equal_Element;

   --== File Operations ==--

   function Read (S : Stream_Access) return PNG_File
   is
      pragma Unreferenced (S);
      Constructed_Chunk_Set : PNG_Chunk_Sets.Set;
      Constructed_PNG_File  : constant PNG_File :=
        (Chunks => Constructed_Chunk_Set);
   begin
      return Constructed_PNG_File;
   end Read;

   procedure Write (S : Stream_Access; F : PNG_File)
   is
      pragma Unreferenced (F);
   begin
      Unsigned_64'Output (S, Signature);
      null;
   end Write;
end PNG;
