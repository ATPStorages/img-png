with Interfaces; use Interfaces;

with IDAT;

package body fcTL is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
      Unsigned_32_Buffer : Unsigned_32;
   begin
      if
        PNG.Chunk_Count (V, IDAT.TypeRaw) = 0 and then
        PNG.Chunk_Count (V, TypeRaw) > 0
      then
         raise PNG.BAD_STRUCTURE_ERROR
           with "Only one fcTL chunk may appear behind the first IDAT chunk";
      end if;

      -- See ihdr.adb

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FramePosition := PNG.Unsigned_31 (Unsigned_32_Buffer);

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FrameWidth := PNG.Unsigned_31_Positive (Unsigned_32_Buffer);

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FrameHeight := PNG.Unsigned_31_Positive (Unsigned_32_Buffer);

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FrameOffsetX := PNG.Unsigned_31 (Unsigned_32_Buffer);

      Unsigned_32'Read (S, Unsigned_32_Buffer);
      PNG.Unsigned_32_ByteFlipper.FlipBytesBE (Unsigned_32_Buffer);
      Self.FrameOffsetY := PNG.Unsigned_31 (Unsigned_32_Buffer);

      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.DelayNumerator);
      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.DelayDenominator);

      Disposal_Operation'Read (S, Self.DisposalOperation);
      if not Self.DisposalOperation'Valid then
         raise INVALID_OPERATION_ERROR
           with "DisposalOperation (dispose_op) of "
           & Self.DisposalOperation'Enum_Rep'Image &
           " is outside specification bounds";
      end if;

      Blend_Operation'Read (S, Self.BlendOperation);

      if not Self.BlendOperation'Valid then
         raise INVALID_OPERATION_ERROR
           with "BlendOperation (blend_op) of "
           & Self.BlendOperation'Enum_Rep'Image &
           " is outside specification bounds";
      end if;
   end Decode;

end fcTL;
