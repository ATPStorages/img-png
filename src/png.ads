with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with ByteFlip;

package PNG is

   --  PNG file format signature.
   Signature : constant Unsigned_64 := 16#89504E470D0A1A0A#;

   --== Chunk Base Defintions ==--

   type Compression_Method is (DEFLATE)
     with Size => 8;

   for Compression_Method use (DEFLATE => 0);

   subtype Chunk_Type is Unsigned_32;
   Chunk_Type_Size_Bytes : constant := Chunk_Type'Size / 8;

   type Chunk_Type_Info is record
      Raw        : Chunk_Type;
      Ancillary  : Boolean;
      PrivateUse : Boolean;
      Reserved   : Boolean;
      SafeToCopy : Boolean;
   end record;

   procedure Create_Type_Info (Info : out Chunk_Type_Info;
                               Raw : Chunk_Type);

   procedure Hydrate_Type_Info (Info : in out Chunk_Type_Info);

   type Unsigned_31 is mod 2 ** 31
     with Size => 32;
   subtype Unsigned_31_Positive is
     PNG.Unsigned_31 range 1 .. PNG.Unsigned_31'Last;

   type Chunk_Data_Array is array (Unsigned_31 range <>) of Unsigned_8;

   type Chunk_Data_Definition is tagged null record;
   type Chunk_Data_Access is
     access all Chunk_Data_Definition'Class;

   type Chunk_Data is record
      Info : Chunk_Data_Access;
   end record;

   type Chunk (Length : Unsigned_31) is record
      TypeInfo  : Chunk_Type_Info;
      Data      : Chunk_Data;
      CRC32     : Unsigned_32;
      FileIndex : Positive_Count;
   end record;

   package Chunk_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Chunk);

   function Chunk_Count (V : Chunk_Vectors.Vector;
                         ChunkType : Chunk_Type)
                         return Natural;

   procedure Decode (Self : in out Chunk_Data_Definition;
                     S : Stream_Access;
                     C : PNG.Chunk;
                     V : Chunk_Vectors.Vector;
                     F : File_Type);

   --== File Reading ==--

   --= Exceptions =--

   --  There's a problem with the first 4 bytes of the PNG stream;
   --  they don't line up with PNG.Signature.
   BAD_SIGNATURE_ERROR : exception;

   --  There's a second chunk where only one chunk of a certain type may exist.
   DUPLICATE_CHUNK_ERROR : exception;

   --  There's a problem with the structure of the PNG stream
   --  (i.e. no IHDR at the start, no IEND at the end)
   BAD_STRUCTURE_ERROR : exception;

   --  There's an unrecognized non-ancillary chunk which cannot be skipped over
   UNRECOGNIZED_CRITICAL_CHUNK_ERROR : exception;

   --  A chunk has an incorrect CRC32.
   CORRUPT_CHUNK_ERROR : exception;

   package Unsigned_16_ByteFlipper is new
     ByteFlip (Modular_Type => Unsigned_16);

   package Unsigned_32_ByteFlipper is new
     ByteFlip (Modular_Type => Unsigned_32);

   package Unsigned_64_ByteFlipper is new
     ByteFlip (Modular_Type => Unsigned_64);

   function Decode_Null_String (S : Stream_Access;
                                Offset : in out Natural)
                                return Unbounded_String;

   function Decode_String_Chunk_End (S : Stream_Access;
                                     F : File_Type;
                                     C : Chunk)
                                     return String;

   --== PNG File Defintion ==--

   type File is record
      Chunks : Chunk_Vectors.Vector;
   end record;

   --  Reads an image from a PNG file.
   --  This will not close the provided stream after finishing.
   function  Read  (F : File_Type; S : Stream_Access)
                    return File;
   procedure Write (F : File;  S : Stream_Access);
private
   function Shr (V : Unsigned_128; Amount : Natural)
                 return Unsigned_128
                 renames Shift_Right;

   function CheckBit5 (N : Unsigned_8)
                       return Boolean;
end PNG;
