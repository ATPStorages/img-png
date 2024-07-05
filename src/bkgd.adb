with IHDR; use IHDR;
with IDAT;
with PLTE;

package body bKGD is

   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Data_Definition) is
   begin
      --  Skip over reading access type in base bKGD.Chunk_Data_Info
      null;
   end Read;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type) is
      IHDRData : constant IHDR.Data_Definition_Access :=
        IHDR.Data_Definition_Access (V.First_Element.Data.Info);
   begin
      if PNG.Chunk_Count (V, bKGD.TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "There may only be one bKGD chunk";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The bKGD chunk must come before the first IDAT chunk";
      elsif
        IHDRData.ColorType = IHDR.INDEXED_COLOR and then
        PNG.Chunk_Count (V, PLTE.TypeRaw) = 0
      then
         raise PNG.BAD_STRUCTURE_ERROR
         with "The bKGD chunk must come after the PLTE chunk in indexed-color";
      end if;

      case IHDRData.ColorType is
         when IHDR.INDEXED_COLOR =>
            declare
               NewbKGD : constant Palette_Definition_Access :=
                 new Palette_Definition;
            begin
               Palette_Definition'Read (S, NewbKGD.all);
            end;
         when IHDR.GRAYSCALE | IHDR.GRAYSCALE_WITH_ALPHA =>
            declare
               NewbKGD : constant Grayscale_Definition_Access :=
                 new Grayscale_Definition;
            begin
               Grayscale_Definition'Read (S, NewbKGD.all);
            end;
         when IHDR.TRUECOLOR | IHDR.TRUECOLOR_WITH_ALPHA =>
            declare
               NewbKGD : constant Truecolor_Definition_Access :=
                 new Truecolor_Definition;
            begin
               Truecolor_Definition'Read (S, NewbKGD.all);
            end;
      end case;
   end Decode;

end bKGD;
