with IDAT;

package body fcTL is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if
        PNG.Chunk_Count (V, IDAT.TypeRaw) = 0 and then
        PNG.Chunk_Count (V, TypeRaw) > 0
      then
         raise PNG.BAD_STRUCTURE_ERROR
           with "Only one fcTL chunk may appear behind the first IDAT chunk";
      end if;

      Data_Definition'Read (S, Self);

      if not Self.DisposalOperation'Valid then
         raise INVALID_OPERATION_ERROR
           with "DisposalOperation (dispose_op) of "
           & Self.DisposalOperation'Enum_Rep'Image &
           " is outside specification bounds";
      elsif not Self.BlendOperation'Valid then
         raise INVALID_OPERATION_ERROR
           with "BlendOperation (blend_op) of "
           & Self.BlendOperation'Enum_Rep'Image &
           " is outside specification bounds";
      end if;

      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FramePosition);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FrameWidth);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FrameHeight);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FrameOffsetX);
      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FrameOffsetY);
      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.DelayNumerator);
      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.DelayDenominator);
   end Decode;

end fcTL;
