with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with PNG; use PNG;

package body tIME is

   overriding procedure Decode (Self : in out Chunk_Data_Info; 
                                S : Stream_Access;
                                C : PNG.Chunk; 
                                V : PNG.Chunk_Vectors.Vector; 
                                F : File_Type) is
   begin
      --  if C.ChunkSize /= 13 then
      --     raise PNG.BAD_CHUNK_SIZE_ERROR with "IHDR size of (" & C.ChunkSize'Image & " ) bytes incorrect, should be 13";
      --  elsif V.Length > 0 then
      --     raise PNG.DUPLICATE_CHUNK_ERROR with "A valid PNG stream must contain only 1 IHDR chunk";
      --  end if;

      Chunk_Data_Info'Read (S, Self);
      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.Year);
      
      Ada.Text_IO.Put_Line ("      tIME Last Modified Date : " & Self.Year'Image & " /" & Self.Month'Image & " /" & Self.Day'Image);
      Ada.Text_IO.Put_Line ("      tIME Last Modified Time : " & Self.Hour'Image & " :" & Self.Minute'Image & " :" & Self.Second'Image);
   end Decode;

end tIME;
