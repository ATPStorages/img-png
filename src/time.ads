with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package tIME is
   
   type Chunk_Data_Info is new PNG.Chunk_Data_Info with record
      Year   : Unsigned_16;
      Month  : Unsigned_8 range 1 .. 12;
      Day    : Unsigned_8 range 1 .. 31;
      Hour   : Unsigned_8 range 0 .. 23;
      Minute : Unsigned_8 range 0 .. 59;
      Second : Unsigned_8 range 0 .. 60;
   end record;
   
   overriding procedure Decode (Self : in out Chunk_Data_Info; 
                                S : Stream_Access;
                                C : PNG.Chunk; 
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end tIME;
