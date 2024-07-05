pragma Ada_2022;

with Ada.Characters.Latin_1;
with IHDR;
with PLTE;
with IDAT;
with tIME;
with iCCP;
with bKGD;
with pHYs;
with iTXt;
with tEXt;
with eXIf;
with acTL;
with fcTL;
with fdAT;

package body PNG is

   function Chunk_Count (V : Chunk_Vectors.Vector;
                         ChunkType : Chunk_Type) return Natural
   is
      Count : Natural := 0;
   begin
      for Chunk of V loop
         if Chunk.TypeInfo.Raw = ChunkType then
            Count := Count + 1;
         end if;
      end loop;

      return Count;
   end Chunk_Count;

   function CheckBit5 (N : Unsigned_8) return Boolean
   is
   begin
      return (N and (2 ** 5)) > 0;
   end CheckBit5;

   procedure Hydrate_Type_Info (Info : in out Chunk_Type_Info)
   is
   begin
      Create_Type_Info (Info, Info.Raw);
   end Hydrate_Type_Info;

   procedure Create_Type_Info (Info : out Chunk_Type_Info;
                               Raw : Chunk_Type)
   is
      RawH : constant Unsigned_128 := Unsigned_128 (Raw);
   begin
      Info.Raw           := Raw;
      Info.Ancillary     := CheckBit5 (Unsigned_8 (Shr (RawH, 24) rem 2 ** 8));
      Info.PrivateUse := CheckBit5 (Unsigned_8 (Shr (RawH, 16) rem 2 ** 8));
      Info.Reserved      := CheckBit5 (Unsigned_8 (Shr (RawH,  8) rem 2 ** 8));
      Info.SafeToCopy    := CheckBit5 (Unsigned_8      (Raw      rem 2 ** 8));
   end Create_Type_Info;

   --== File Operations ==--

   function Decode_Null_String (S : Stream_Access;
                                Offset : in out Natural)
                                return Unbounded_String
   is
      New_String     : Unbounded_String;
      Read_Character : Character;
   begin
      while True loop
         Character'Read (S, Read_Character);
         Offset := @ + 1;

         if Read_Character = Ada.Characters.Latin_1.NUL then
            exit;
         else
            Append (New_String, Read_Character);
         end if;
      end loop;

      return New_String;
   end Decode_Null_String;

   function Decode_String_Chunk_End (S : Stream_Access;
                                     F : File_Type;
                                     C : Chunk)
                                     return String
   is
      New_String : String (1 .. Natural (C.Length) - Natural (Index (F) - C.FileIndex));
   begin
      String'Read (S, New_String);
      return New_String;
   end Decode_String_Chunk_End;

   procedure Decode (Self : in out Chunk_Data_Definition;
                     S : Stream_Access;
                     C : Chunk;
                     V : Chunk_Vectors.Vector;
                     F : File_Type)
   is
      discard : Chunk_Data_Array (1 .. C.Length);
   begin
      Chunk_Data_Array'Read (S, discard);
   end Decode;

   function Read (F : File_Type; S : Stream_Access) return File
   is
      Stream_Signature : Unsigned_64;
      Stream_Ended     : Boolean := False;

      IHDR_Present : Boolean := False;
      Chnk_Length  : Unsigned_31;
      Chnk_Index   : Positive_Count;

      New_File : aliased File;
      Chunks   : Chunk_Vectors.Vector renames New_File.Chunks;
   begin
      Unsigned_64'Read (S, Stream_Signature);
      Unsigned_64_ByteFlipper.FlipBytesBE (Stream_Signature);
      if Stream_Signature /= Signature then
         raise BAD_SIGNATURE_ERROR;
      end if;

      while True loop
         if Stream_Ended then
            if End_Of_File (F) then
               exit;
            else
               raise BAD_STRUCTURE_ERROR
                 with "IEND must appear at the very end of a PNG stream";
            end if;
         end if;

         Unsigned_31'Read (S, Chnk_Length);
         Unsigned_31_ByteFlipper.FlipBytesBE (Chnk_Length);

         declare
            --  Computed_CRC32          : Unsigned_32;
            Constructed_Chunk       : Chunk (Chnk_Length);
         begin
            Chunk_Type'Read
              (S, Constructed_Chunk.TypeInfo.Raw);
            Unsigned_32_ByteFlipper.FlipBytesBE
              (Constructed_Chunk.TypeInfo.Raw);

            Hydrate_Type_Info (Constructed_Chunk.TypeInfo);

            Constructed_Chunk.FileIndex := Index (F);

            if
              IHDR_Present = False and then
              Chunks.Length > 0 and then
              Chunk_Count (Chunks, IHDR.TypeRaw) = 0
            then
               raise BAD_STRUCTURE_ERROR
                 with "The IHDR chunk must come first in a PNG datastream";
            end if;

            case Constructed_Chunk.TypeInfo.Raw is
               when IHDR.TypeRaw =>
                  Constructed_Chunk.Data.Info := new IHDR.Data_Definition;
                  IHDR_Present := True;
               when PLTE.TypeRaw =>
                  Constructed_Chunk.Data.Info := new PLTE.Data_Definition
                    (Chnk_Length, Chnk_Length / 3);
               when IDAT.TypeRaw =>
                  Constructed_Chunk.Data.Info := new IDAT.Data_Definition
                    (Chnk_Length);
               when 16#49454E44# => --  IEND
                  Stream_Ended := True;
                  goto NoDecode;

               when tIME.TypeRaw =>
                  Constructed_Chunk.Data.Info := new tIME.Data_Definition;
               when iCCP.TypeRaw =>
                  Constructed_Chunk.Data.Info := new iCCP.Data_Definition;

               when bKGD.TypeRaw =>
                  Constructed_Chunk.Data.Info := new bKGD.Data_Definition;
               when pHYs.TypeRaw =>
                  Constructed_Chunk.Data.Info := new pHYs.Data_Definition;
               when iTXt.TypeRaw =>
                  Constructed_Chunk.Data.Info := new iTXt.Data_Definition;
               when tEXt.TypeRaw =>
                  Constructed_Chunk.Data.Info := new tEXt.Data_Definition;
               when eXIf.TypeRaw =>
                  Constructed_Chunk.Data.Info := new eXIf.Data_Definition;

               when acTL.TypeRaw => --  APNG related chunks
                  Constructed_Chunk.Data.Info := new acTL.Data_Definition;
               when fcTL.TypeRaw =>
                  Constructed_Chunk.Data.Info := new fcTL.Data_Definition;
               when fdAT.TypeRaw =>
                  Constructed_Chunk.Data.Info := new fdAT.Data_Definition
                    (Chnk_Length - (Unsigned_31'Size / 8));

               when others =>
                  if not Constructed_Chunk.TypeInfo.Ancillary then
                     raise UNRECOGNIZED_CRITICAL_CHUNK_ERROR
                     with "Last Read:" & Chunks.Last_Element.TypeInfo.Raw'Image & " File Index @" & Index (F)'Image;
                  end if;

                  Constructed_Chunk.Data.Info := new Chunk_Data_Definition;
            end case;

            Decode (Constructed_Chunk.Data.Info.all,
                    S,
                    Constructed_Chunk,
                    Chunks,
                    F);
            <<NoDecode>>

            Unsigned_32'Read (S, Constructed_Chunk.CRC32);
            Unsigned_32_ByteFlipper.FlipBytesBE (Constructed_Chunk.CRC32);

            Chunks.Append (Constructed_Chunk);
         end;
      end loop;

      return New_File;
   end Read;

   procedure Write (F : File; S : Stream_Access)
   is
      pragma Unreferenced (F);
   begin
      Unsigned_64'Output (S, Signature);
      null;
   end Write;
end PNG;
