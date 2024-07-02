with Ada.Characters.Latin_1;
with Ada.Containers; use Ada.Containers;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO;
with IHDR;
with PNG; use PNG;

package body bKGD is

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Chunk_Data_Info) is
   begin
      --  Skip over reading access type in base bKGD.Chunk_Data_Info
      null;
   end Read;
   
   overriding procedure Decode (Self : in out Chunk_Data_Info; 
                                S : Stream_Access; 
                                C : PNG.Chunk; 
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type) is
      IHDR_Data : IHDR.Chunk_Data_Info_Access := IHDR.Chunk_Data_Info_Access (V.First_Element.Data.Info);
   begin
      --  if C.ChunkSize /= 13 then
      --     raise PNG.BAD_CHUNK_SIZE_ERROR with "IHDR size of (" & C.ChunkSize'Image & " ) bytes incorrect, should be 13";
      --  elsif V.Length > 0 then
      --     raise PNG.DUPLICATE_CHUNK_ERROR with "A valid PNG stream must contain only 1 IHDR chunk";
      --  end if;
      
      case IHDR_Data.ColorType is
         when IHDR.INDEXED_COLOR =>
            declare
               New_bKGD : Palette_Chunk_Data_Info_Access := new Palette_Chunk_Data_Info;
            begin
               Palette_Chunk_Data_Info'Read (S, New_bKGD.all);
               Ada.Text_IO.Put_Line ("      bKGD Palette Index : " & New_bKGD.Index'Image);
            end;
         when IHDR.GRAYSCALE | IHDR.GRAYSCALE_WITH_ALPHA =>
            declare
               New_bKGD : Grayscale_Chunk_Data_Info_Access := new Grayscale_Chunk_Data_Info;
            begin
               Grayscale_Chunk_Data_Info'Read (S, New_bKGD.all);
               Ada.Text_IO.Put_Line ("      bKGD Gray Level : " & New_bKGD.GrayLevel'Image);
            end;
         when IHDR.TRUECOLOR | IHDR.TRUECOLOR_WITH_ALPHA =>
            declare
               New_bKGD : Truecolor_Chunk_Data_Info_Access := new Truecolor_Chunk_Data_Info;
            begin
               Truecolor_Chunk_Data_Info'Read (S, New_bKGD.all);
               Ada.Text_IO.Put_Line ("      bKGD R : " & New_bKGD.R'Image);
               Ada.Text_IO.Put_Line ("      bKGD G : " & New_bKGD.G'Image);
               Ada.Text_IO.Put_Line ("      bKGD B : " & New_bKGD.B'Image);
            end;
      end case;
   end Decode;

end bKGD;
