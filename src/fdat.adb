with IDAT;

package body fdAT is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, IDAT.TypeRaw) < 1 then
         raise PNG.BAD_STRUCTURE_ERROR
         with "All fdAT chunks must come after the inital IDAT chunk";
      end if;

      Data_Definition'Read (S, Self);

      PNG.Unsigned_31_ByteFlipper.FlipBytesBE (Self.FramePosition);
   end Decode;

end fdAT;
