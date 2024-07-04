package body tIME is

   overriding procedure Decode (Self : in out Data_Definition;
                                S : Stream_Access;
                                C : PNG.Chunk;
                                V : PNG.Chunk_Vectors.Vector;
                                F : File_Type)
   is
   begin
      if PNG.Chunk_Count (V, 16#74494D45#) > 0 then
         raise PNG.DUPLICATE_CHUNK_ERROR with
           "A valid PNG stream must contain only 1 tIME chunk";
      end if;

      Data_Definition'Read (S, Self);
      PNG.Unsigned_16_ByteFlipper.FlipBytesBE (Self.Year);
   end Decode;

end tIME;
