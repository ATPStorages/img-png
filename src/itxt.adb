
package body iTXt is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Offset : Natural := 0;
   begin
      Self.Keyword := PNG.Decode_Null_String (S, Offset);
      Boolean'Read (S, Self.Compressed);
      PNG.Compression_Method'Read (S, Self.CompressionMethod);
      Self.LanguageTag := PNG.Decode_Null_String (S, Offset);
      Self.TranslatedKeyword := PNG.Decode_Null_String (S, Offset);
      Self.Text := To_Unbounded_String (PNG.Decode_String_Chunk_End (S, F, C));
   end Decode;

end iTXt;
