with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with PNG;

package fcTL is

   TypeRaw : constant PNG.Chunk_Type := 16#6663544C#;

   type Disposal_Operation is (APNG_DISPOSE_OP_NONE,
                              APNG_DISPOSE_OP_BACKGROUND,
                              APNG_DISPOSE_OP_PREVIOUS)
     with Size => 8;

   for Disposal_Operation use (APNG_DISPOSE_OP_NONE => 0,
                              APNG_DISPOSE_OP_BACKGROUND => 1,
                              APNG_DISPOSE_OP_PREVIOUS => 2);

   type Blend_Operation is (APNG_BLEND_OP_SOURCE,
                           APNG_BLEND_OP_OVER)
     with Size => 8;

   for Blend_Operation use (APNG_BLEND_OP_SOURCE => 0,
                           APNG_BLEND_OP_OVER => 1);

   INVALID_OPERATION_ERROR : exception;

   type Data_Definition is new PNG.Chunk_Data_Definition with record
      FramePosition     : PNG.Unsigned_31;
      FrameWidth        : PNG.Unsigned_31_Positive;
      FrameHeight       : PNG.Unsigned_31_Positive;
      FrameOffsetX      : PNG.Unsigned_31;
      FrameOffsetY      : PNG.Unsigned_31;
      DelayNumerator    : Unsigned_16;
      DelayDenominator  : Unsigned_16;
      DisposalOperation : Disposal_Operation;
      BlendOperation    : Blend_Operation;
   end record;

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : in out PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type);

end fcTL;
