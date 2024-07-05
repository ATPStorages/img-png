with PNG;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package sRGB is

   TypeRaw : constant PNG.Chunk_Type := 16#73524742#;

   type Rendering_Intent is (PERCEPTUAL,
                            RELATIVE_COLORIMETRIC,
                            SATURATION,
                            ABSOLUTE_COLORIMETRIC)
     with Size => 8;

   for Rendering_Intent use (PERCEPTUAL => 0,
                             RELATIVE_COLORIMETRIC => 1,
                             SATURATION => 2,
                             ABSOLUTE_COLORIMETRIC => 3);

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      RenderingIntent : Rendering_Intent;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end sRGB;
