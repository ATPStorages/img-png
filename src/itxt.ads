with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with PNG;

package iTXt is

   TypeRaw : constant PNG.Chunk_Type := 16#69545874#;

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      Keyword           : Unbounded_String;
      Compressed        : Boolean;
      CompressionMethod : PNG.Compression_Method;
      LanguageTag       : Unbounded_String;
      TranslatedKeyword : Unbounded_String;
      Text              : Unbounded_String;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end iTXt;
