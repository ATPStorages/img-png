with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with PNG; use PNG;

package body tEXt is

   overriding procedure Decode (Self : in out Chunk_Data_Info; S : Stream_Access; C : PNG.Chunk; V : PNG.Chunk_Vectors.Vector; F : File_Type) is
   begin
      --  if C.ChunkSize /= 13 then
      --     raise PNG.BAD_CHUNK_SIZE_ERROR with "IHDR size of (" & C.ChunkSize'Image & " ) bytes incorrect, should be 13";
      --  elsif V.Length > 0 then
      --     raise PNG.DUPLICATE_CHUNK_ERROR with "A valid PNG stream must contain only 1 IHDR chunk";
      --  end if;

      Self.Keyword := PNG.Decode_Null_String (S);
      declare
         Text : String (1 .. Natural (C.Length) - Length (Self.Keyword) - 1);
      begin
         String'Read (S, Text);
         Append (Self.Text, Text);
      end;
      
      Ada.Text_IO.Put_Line ("      tEXt Keyword : " & To_String (Self.Keyword));
      Ada.Text_IO.Put_Line ("      tEXt String  : " & To_String (Self.Text));
   end Decode;

end tEXt;
