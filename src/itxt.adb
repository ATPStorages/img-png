with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with PNG; use PNG;

package body iTXt is

   overriding procedure Decode (Self : in out Chunk_Data_Info;
                                S : Stream_Access; 
                                C : PNG.Chunk; 
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type) 
   is
      Start_File_Position : Positive_Count := Index (F);
      Last_File_Position  : Positive_Count;
   begin
      --  if C.ChunkSize /= 13 then
      --     raise PNG.BAD_CHUNK_SIZE_ERROR with "IHDR size of (" & C.ChunkSize'Image & " ) bytes incorrect, should be 13";
      --  elsif V.Length > 0 then
      --     raise PNG.DUPLICATE_CHUNK_ERROR with "A valid PNG stream must contain only 1 IHDR chunk";
      --  end if;

      Self.Keyword           := PNG.Decode_Null_String (S);
      Boolean'Read (S, Self.Compressed);
      CompressionMethods'Read (S, Self.CompressionMethod);
      Self.LanguageTag       := PNG.Decode_Null_String (S);
      Self.TranslatedKeyword := PNG.Decode_Null_String (S);
      Last_File_Position     := Index (F);
      
      declare
         Text : String (1 .. Natural (C.Length) - Natural (Last_File_Position - Start_File_Position));
      begin
         Ada.Text_IO.Put_Line (Natural (Last_File_Position - Start_File_Position)'Image);
         Ada.Text_IO.Put_Line (Natural (C.Length)'Image);
         
         String'Read (S, Text);
         Append (Self.Text, Text);
      end;
      
      Ada.Text_IO.Put_Line ("      iTXt Keyword            : " & To_String (Self.Keyword));
      Ada.Text_IO.Put_Line ("      iTXt Compressed?        : " & Self.Compressed'Image);
      Ada.Text_IO.Put_Line ("      iTXt Compression Method : " & Self.CompressionMethod'Image);
      Ada.Text_IO.Put_Line ("      iTXt Language Key       : " & To_String (Self.LanguageTag));
      Ada.Text_IO.Put_Line ("      iTXt Translated Keyword : " & To_String (Self.TranslatedKeyword));
      Ada.Text_IO.Put_Line ("      iTXt String             : " & To_String (Self.Text));
   end Decode;

end iTXt;
