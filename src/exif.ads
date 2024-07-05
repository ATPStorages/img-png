with Ada.Containers.Indefinite_Vectors;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package eXIf is
   
   TypeRaw : constant PNG.Chunk_Type := 16#65584966#;

   type Tag_Data_Type is (UNSIGNED_BYTE,
                         ASCII,
                         UNSIGNED_SHORT,
                         UNSIGNED_LONG,
                         RATIONAL,
                         SIGNED_BYTE,
                         UNDEFINED,
                         SIGNED_SHORT,
                         SIGNED_LONG,
                         SIGNED_RATIONAL,
                         FLOAT,
                         DOUBLE)
     with Size => 16;

   for Tag_Data_Type use (UNSIGNED_BYTE => 1,
                         ASCII => 2,
                         UNSIGNED_SHORT => 3,
                         UNSIGNED_LONG => 4,
                         RATIONAL => 5,
                         SIGNED_BYTE => 6,
                         UNDEFINED => 7,
                         SIGNED_SHORT => 8,
                         SIGNED_LONG => 9,
                         SIGNED_RATIONAL => 10,
                         FLOAT => 11,
                         DOUBLE => 12);

   type Tag is tagged record
      ID             : Unsigned_16;
      DataType       : Tag_Data_Type;
      ValueCount     : Unsigned_32;
      ValueOrPointer : Unsigned_32;
   end record;

   type Tag_Array is array (Unsigned_16 range <>) of Tag;

   type Image_File_Directory (TagCount : Unsigned_16) is record
      Tags : Tag_Array (1 .. TagCount);
   end record;

   package Image_File_Directory_Vectors is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Image_File_Directory);

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      ImageFileDirectories : Image_File_Directory_Vectors.Vector;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end eXIf;
