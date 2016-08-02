-- File: adaid-generate.adb
-- Description: UUID Generation
-- Author: Anthony Arnold
-- License: Simplified BSD License (see LICENSE)

with SHA.Process_Data;  -- For From_Name
with Ada.Numerics.Discrete_Random; -- For RNG
with Ada.Streams.Stream_IO; -- For reading from /dev/random
use Ada.Streams.Stream_IO;

package body AdaID.Generate is

   -- For RNG
   package RNG is new Ada.Numerics.Discrete_Random (Result_Subtype =>
                                                       Unsigned_32);
   generator : RNG.Generator;
   generator_is_set : Boolean := False;


   procedure Seed_RNG is
      seed : Integer;
      file : File_Type;
   begin
      -- Set up the generator first time
      if not generator_is_set then
         begin
            Open   (File => file,
                    Mode => In_File,
                    Name => "/dev/urandom");
            Integer'Read (Stream (file), seed);
            Close (file);
            RNG.Reset (generator, seed);
         exception
            when others =>
               RNG.Reset (generator); -- Fallback to time-based
         end;
         generator_is_set := True;
      end if;
   end Seed_RNG;


   -- Reset a UUID to Nil
   procedure Nil (id : in out UUID) is
   begin
      Initialize (id);
   end Nil;


   -- Generate a random UUID
   procedure Random (id : in out UUID) is
      rand : Unsigned_32;
      x : Integer := 0;
   begin
      -- Ensure RNG is seeded
      Seed_RNG;

      -- Get a random number
      rand := RNG.Random (generator);

      for i in ByteArray'Range loop
         if x = 4 then
            x := 0;
            rand := RNG.Random (generator);
         end if;
         id.data (i) := Byte (shift_right (rand, x * 8) and 16#FF#);
         x := x + 1;
      end loop;

      -- Set the variant
      id.data (8) := (id.data (8) and 16#BF#) or 16#80#;

      -- Set the version to random-number-based
      id.data (6) := (id.data (6) and 16#4F#) or 16#40#;
   end Random;


   -- Generate a UUID based on a name
   procedure From_Name
     (namespace : in UUID;
      name      : in String;
      id        : in out UUID)
   is
      use SHA.Process_Data;

      c : Context;
      d : SHA.Digest;
   begin
      Initialize (c);

      -- Start SHA1 hashing with namespace uuid
      for i in namespace.data'Range loop
         Add (SHA.Process_Data.Byte (namespace.data (i)), c);
      end loop;

      -- Continuing hashing the actual name
      for i in name'Range loop
         Add (SHA.Process_Data.Byte (Character'Pos (name (i))), c);
      end loop;

      -- Get the digest
      Finalize (d, c);

      -- Now make the UUID from the hash
      for i in 0 .. 3 loop
         id.data (i * 4 + 0) := Byte (shift_right (d (i), 24) and 16#FF#);
         id.data (i * 4 + 1) := Byte (shift_right (d (i), 16) and 16#FF#);
         id.data (i * 4 + 2) := Byte (shift_right (d (i),  8) and 16#FF#);
         id.data (i * 4 + 3) := Byte (shift_right (d (i),  0) and 16#FF#);
      end loop;

      -- set variant
      id.data (8) := (id.data (8) and 16#BF#) or 16#80#;

      -- set version
      id.data (6) := (id.data (6) and 16#5F#) or 16#50#;
   end From_Name;


   -- Generate a UUID from a string.
   -- This is not so much generation, but reconstruction
   procedure From_String (str : in String; id : in out UUID) is
      delim : constant Character := '-';
      open : constant Character := '{';
      close : constant Character := '}';

      -- expect dashes if str is 36 or 38 in length
      dashed : constant Boolean := str'Length = 36 or else str'Length = 38;

      -- check to see if braces surround the string
      braced : constant Boolean := str (str'First) = open and then
        str (str'Last) = close;

      -- track where to read from/write to
      idx : Integer := 0;
      start, rel : Integer;
   begin

      -- Check that length is valid
      if not dashed and then str'Length /= 32 and then str'Length /= 34 then
         raise Invalid_String;
      end if;

      -- Check that brace are valid
      start := str'First;
      if not braced and then
        (str (str'First) = open or else str (str'Last) = close)
      then
         raise Invalid_String; -- only one brace present
      elsif braced then
         start := str'First + 1;
      end if;

      idx := start;

      -- Grab each pair and stuff into byte
      for i in ByteArray'Range loop
         rel := idx - start;
         if dashed and then
           (rel = 8 or else rel = 13 or else rel = 18 or else rel = 23)
         then
            if str (idx) /= delim then
               raise Invalid_String; -- expected '-'
            end if;
            idx := idx + 1;
         end if;
         -- Convert to byte
         id.data (i) := Byte'Value ("16#" & str (idx .. idx + 1) & "#");
         idx := idx + 2;
      end loop;
   end From_String;

end AdaID.Generate;
