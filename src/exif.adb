with Ada.Containers;

with IDAT;

package body eXIf is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      ForBigEndian     : Boolean;
      Unsigned16Buffer : Unsigned_16;
      Unsigned32Buffer : Unsigned_32;
      DataStart        : constant Positive_Count := Index (F);
   begin
      if PNG.Chunk_Count (V, TypeRaw) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR
         with "Only one eXIf chunk may exist";
      elsif PNG.Chunk_Count (V, IDAT.TypeRaw) > 0 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "eXIf must come before the first IDAT chunk";
      end if;

      --  Reading endianness
      Unsigned_16'Read (S, Unsigned16Buffer);
      ForBigEndian := Unsigned16Buffer = 16#4D4D#;
      --  TIFF Marker (0x002A for EXIF)
      Unsigned_16'Read (S, Unsigned16Buffer);
      --  First IFD Pointer
      Unsigned_32'Read (S, Unsigned32Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesCHK
        (Unsigned32Buffer, ForBigEndian);
      Set_Index (F, DataStart + Positive_Count (Unsigned32Buffer));

      while True loop
         Unsigned_16'Read (S, Unsigned16Buffer);
         PNG.Unsigned_16_ByteFlipper.FlipBytesCHK
           (Unsigned16Buffer, ForBigEndian);

         declare
            NewImageFileDirectory : Image_File_Directory (Unsigned16Buffer);
            IteratedTag           : Tag;
         begin
            for TagIndex in NewImageFileDirectory.Tags'Range loop
               IteratedTag := NewImageFileDirectory.Tags (TagIndex);
               Tag'Read (S, IteratedTag);
               PNG.Unsigned_16_ByteFlipper.FlipBytesCHK
                 (IteratedTag.ID, ForBigEndian);

               --  EnumerationValue := NewImageFileDirectory.Tags (TagIndex).DataType'Enum_Rep;
               --  PNG.Unsigned_32_ByteFlipper.FlipBytesCHK (EnumerationValue, ForBigEndian);
               --  NewImageFileDirectory.Tags (TagIndex).DataType := TagDataTypes'Val (EnumerationValue);

               PNG.Unsigned_32_ByteFlipper.FlipBytesCHK
                 (IteratedTag.ValueCount, ForBigEndian);
               PNG.Unsigned_32_ByteFlipper.FlipBytesCHK
                 (IteratedTag.ValueOrPointer, ForBigEndian);
            end loop;
            Self.ImageFileDirectories.Append (NewImageFileDirectory);
         end;

         Unsigned_32'Read (S, Unsigned32Buffer);
         PNG.Unsigned_32_ByteFlipper.FlipBytesCHK
           (Unsigned32Buffer, ForBigEndian);

         if Unsigned32Buffer > 0 then
            Set_Index (F, DataStart + Positive_Count (Unsigned32Buffer));
         else
            exit;
         end if;
      end loop;
   end Decode;

end eXIf;
