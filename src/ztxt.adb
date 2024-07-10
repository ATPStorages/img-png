package body zTXt is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Offset : Natural := 0;
   begin
      Self.Keyword := PNG.Decode_Null_String (S, Offset);
      PNG.Compression_Method'Read (S, Self.CompressionMethod);
      --
   end Decode;

end zTXt;
