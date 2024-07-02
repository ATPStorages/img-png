with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package bKGD is
   
   type Color_Data_Info_Access is access PNG.Chunk_Data_Info'Class;
   
   type Chunk_Data_Info is new PNG.Chunk_Data_Info with record
      Color_Data_Info : Color_Data_Info_Access;
   end record;
   
   procedure Read
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Chunk_Data_Info);
   
   for Chunk_Data_Info'Read use Read;

   overriding procedure Decode (Self : in out Chunk_Data_Info;
                                S : Stream_Access; 
                                C : PNG.Chunk; 
                                V : PNG.Chunk_Vectors.Vector; 
                                F : File_Type);
   
   type Grayscale_Chunk_Data_Info is new Chunk_Data_Info with record
      GrayLevel : Unsigned_16;
   end record;
   
   type Grayscale_Chunk_Data_Info_Access is access Grayscale_Chunk_Data_Info;
   
   type Truecolor_Chunk_Data_Info is new Chunk_Data_Info with record
      R : Unsigned_16;
      G : Unsigned_16;
      B : Unsigned_16;
   end record;
   
   type Truecolor_Chunk_Data_Info_Access is access Truecolor_Chunk_Data_Info;
   
   type Palette_Chunk_Data_Info is new Chunk_Data_Info with record
      Index : Unsigned_8;
   end record;
   
   type Palette_Chunk_Data_Info_Access is access Palette_Chunk_Data_Info;

end bKGD;
