with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with PNG; use PNG;

package body iCCP is

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

      Self.ProfileName := PNG.Decode_Null_String (S);
      CompressionMethods'Read (S, Self.CompressionMethod);
      Last_File_Position := Index (F);
      
      declare
         Profile : String (1 .. Natural (C.Length) - Natural (Last_File_Position - Start_File_Position));
      begin
         String'Read (S, Profile);
         Append (Self.Profile, Profile);
      end;
      
      Ada.Text_IO.Put_Line ("      iCCP Profile            : " & To_String (Self.ProfileName));
      Ada.Text_IO.Put_Line ("      iCCP Compression Method : " & Self.CompressionMethod'Image);
      --  Ada.Text_IO.Put_Line ("      iCCP Data               : " & To_String (Self.Profile));
   end Decode;

end iCCP;
