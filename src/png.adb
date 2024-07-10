pragma Ada_2022;

with Ada.Characters.Latin_1;
with IHDR;
with PLTE;
with IDAT;
with tIME;
with iCCP;
with cICP;
with bKGD;
with pHYs;
with zTXt;
with iTXt;
with tEXt;
with eXIf;
with acTL;
with fcTL;
with fdAT;

with Checksum.CRC;

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
      New_String : String
        (1 .. Natural (C.Length) - Natural (Index (F) - C.FileIndex));
   begin
      String'Read (S, New_String);
      return New_String;
   end Decode_String_Chunk_End;

   procedure Decode (Self : in out Chunk_Data_Definition;
                     S : Stream_Access;
                     C : in out Chunk;
                     V : Chunk_Vectors.Vector;
                     F : File_Type)
   is
   begin
      null;
   end Decode;

   Local_CRC    : Checksum.CRC.CRC_Array;
   Local_CRC_OK : Boolean := False;

   function Read (F : File_Type;
                  S : Stream_Access;
                  Compute_CRC : Boolean)
                  return File
   is
      Stream_Signature : Unsigned_64;
      Stream_Ended     : Boolean := False;

      IHDR_Present : Boolean := False;
      Chnk_Length  : Unsigned_32;
      Chnk_Index   : Positive_Count;

      New_File : File;
      Chunks   : Chunk_Vectors.Vector renames New_File.Chunks;
   begin
      if not Local_CRC_OK then
         Checksum.CRC.Compute_CRC_Table (Local_CRC);
         Local_CRC_OK := True;
      end if;

      Unsigned_64'Read (S, Stream_Signature);
      Unsigned_64_ByteFlipper.FlipBytesBE (Stream_Signature);
      if Stream_Signature /= Signature then
         raise BAD_SIGNATURE_ERROR;
      end if;

      while True loop
         if
           Stream_Ended and then
           End_Of_File (F)
         then
            exit;
         end if;

         Unsigned_32'Read (S, Chnk_Length);
         Unsigned_32_ByteFlipper.FlipBytesBE (Chnk_Length);

         declare
            Computed_CRC32 : Unsigned_32 := Checksum.Full_32;
            Chunk          : PNG.Chunk (Unsigned_31 (Chnk_Length));
         begin
            if Stream_Ended then
               declare
                  End_Error : Decoder_Error (BAD_ORDER);
               begin
                  End_Error.Constraints.Insert (IEND_Chunk_Type, BEFORE);
                  Chunk.Data.Errors.Append (End_Error);
               end;
            end if;

            Chunk_Type'Read (S, Chunk.TypeInfo.Raw);
            Unsigned_32_ByteFlipper.FlipBytesBE (Chunk.TypeInfo.Raw);

            Hydrate_Type_Info (Chunk.TypeInfo);
            Chunk.FileIndex := Index (F);

            if
              IHDR_Present = False and then
              Chunks.Length > 0 and then
              Chunk_Count (Chunks, IHDR.TypeRaw) = 0
            then
               declare
                  IHDR_Error : Decoder_Error (BAD_ORDER);
               begin
                  IHDR_Error.Constraints.Insert (IHDR.TypeRaw, BEFORE);
                  Chunk.Data.Errors.Append (IHDR_Error);
               end;
            end if;

            case Chunk.TypeInfo.Raw is
               when IHDR.TypeRaw =>
                  Chunk.Data.Info := new IHDR.Data_Definition;
                  IHDR_Present := True;
               when PLTE.TypeRaw =>
                  Chunk.Data.Info := new PLTE.Data_Definition
                    (Unsigned_31 (Chnk_Length), Unsigned_31 (Chnk_Length / 3));
               when IDAT.TypeRaw =>
                  Chunk.Data.Info := new IDAT.Data_Definition
                    (Chnk_Length - 9);
               when IEND_Chunk_Type => --  IEND
                  Stream_Ended := True;
                  goto NoDecode;

               when tIME.TypeRaw =>
                  Chunk.Data.Info := new tIME.Data_Definition;
               when iCCP.TypeRaw =>
                  Chunk.Data.Info := new iCCP.Data_Definition;
               when cICP.TypeRaw =>
                  Chunk.Data.Info := new cICP.Data_Definition;

               when bKGD.TypeRaw =>
                  Chunk.Data.Info := new bKGD.Data_Definition;
               when pHYs.TypeRaw =>
                  Chunk.Data.Info := new pHYs.Data_Definition;
               when zTXt.TypeRaw =>
                  Chunk.Data.Info := new zTXt.Data_Definition
                    (Chnk_Length - 22);
               when iTXt.TypeRaw =>
                  Chunk.Data.Info := new iTXt.Data_Definition;
               when tEXt.TypeRaw =>
                  Chunk.Data.Info := new tEXt.Data_Definition;
               when eXIf.TypeRaw =>
                  Chunk.Data.Info := new eXIf.Data_Definition;

               when acTL.TypeRaw => --  APNG related chunks
                  Chunk.Data.Info := new acTL.Data_Definition;
               when fcTL.TypeRaw =>
                  Chunk.Data.Info := new fcTL.Data_Definition;
               when fdAT.TypeRaw =>
                  Chunk.Data.Info := new fdAT.Data_Definition
                    (Unsigned_31 (Chnk_Length) - 4);

               when others =>
                  if not Chunk.TypeInfo.Ancillary then
                     raise UNRECOGNIZED_CRITICAL_CHUNK_ERROR
                       with "Last Read:" &
                       Chunks.Last_Element.TypeInfo.Raw'Image &
                       " File Index @" & Index (F)'Image;
                  end if;

                  Chunk.Data.Info := new Chunk_Data_Definition;
            end case;

            Decode (Chunk.Data.Info.all, S, Chunk, Chunks, F);
            <<NoDecode>>

            if Compute_CRC then
               declare
                  To_Read        : Unsigned_32 :=
                    Chnk_Length + Chunk_Type_Size_Bytes;
                  Checksum_Error : Decoder_Error (CRC_MISMATCH);
               begin
                  Set_Index (F, Chunk.FileIndex - Chunk_Type_Size_Bytes);

                  loop
                     declare
                        Data : Checksum.Byte_Array
                          (1 .. Unsigned_32'Min (To_Read, 8192));
                     begin
                        Checksum.Byte_Array'Read (S, Data);
                        To_Read := @ - (Data'Size / 8);
                        Checksum.CRC.Update_CRC (Computed_CRC32, Local_CRC, Data);
                     end;

                     exit when To_Read = 0;
                  end loop;

                  Computed_CRC32 := @ xor Checksum.Full_32;

                  Unsigned_32'Read (S, Chunk.CRC32);
                  Unsigned_32_ByteFlipper.FlipBytesBE (Chunk.CRC32);

                  if Computed_CRC32 /= Chunk.CRC32 then
                     Checksum_Error.Read := Computed_CRC32;
                     Chunk.Data.Errors.Append (Checksum_Error);
                  end if;
               end;
            else
               declare
                  No_Checksum_Error : Decoder_Error (CRC_NOT_COMPUTED);
               begin
                  Chunk.Data.Errors.Append (No_Checksum_Error);
               end;

               Set_Index (F, Chunk.FileIndex + Positive_Count (Chunk.Length + 4));
            end if;

            if Chunk.TypeInfo.Raw /= IEND_Chunk_Type then
               Chunks.Append (Chunk);
            end if;
         end;
      end loop;

      return New_File;
   end Read;

   procedure Write (F : File; S : Stream_Access)
   is
      pragma Unreferenced (F);
   begin
      Unsigned_64'Output (S, Signature);
   end Write;
end PNG;
