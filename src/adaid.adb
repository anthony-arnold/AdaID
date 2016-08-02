-- File: adaid.adb
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: Simplified BSD License (see LICENSE)

package body AdaID is
   -- Default "Constructor" for NIL UUID
   overriding procedure Initialize (This : in out UUID) is
   begin
      for i in ByteArray'Range loop
         This.data (i) := 0;
      end loop;
   end Initialize;

   -- Determine if the UUID is NIL
   function Is_Nil (This : in UUID) return Boolean is
   begin
      for i in ByteArray'Range loop
         if This.data (i) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end Is_Nil;

   -- Get the UUID Version
   function Get_Version (This : in UUID) return VersionType is
      -- version type in octet 7
      b : constant Byte := This.data (6) and Byte (16#F0#);
   begin
      case b is
         when 16#10# => return Time_Based;
         when 16#20# => return DCE_Security;
         when 16#30# => return Name_Based_MD5;
         when 16#40# => return Random_Number_Based;
         when 16#50# => return Name_Based_SHA1;
         when others => return Unknown;
      end case;
   end Get_Version;

   -- Get the UUID Variant
   function Get_Variant (This : in UUID) return VariantType is
      -- variant type in octet 9
      b : constant Byte := This.data (8);
   begin
      if (b and 16#80#) = 0 then
         return NCS;
      elsif (b and 16#C0#) = 16#80# then
         return RFC_4122;
      elsif (b and 16#E0#) = 16#C0# then
         return Microsoft;
      else
         return Future;
      end if;
   end Get_Variant;

   -- Test for equality
   function "="(Left, Right : in UUID) return Boolean is
   begin
      for i in ByteArray'Range loop
         if Left.data (i) /= Right.data (i) then
            return False;
         end if;
      end loop;
      return True;
   end "=";


   -- Get the hash value for the UUID
   function Get_Hash_Value (This : in UUID) return HashType is
      seed : HashType := 0;
   begin
      -- Hashing (in case it's needed)
      for i in ByteArray'Range loop
         seed := seed xor
           (
            HashType (This.data (i))
              + 16#9E3779B9#
              + shift_left (seed, 6)
              + shift_right (seed, 2)
           );
      end loop;
      return seed;
   end Get_Hash_Value;

   -- Convert the UUID to a string
   function To_String (This : in UUID) return String is
      result : String (1 .. 36);
      index : Integer := 1;
      base : constant Integer := 2 ** 4;
      chars : constant String (1 .. base) := "0123456789abcdef";
      b : Integer;
   begin
      for i in ByteArray'Range loop
         b := Integer (This.data (i)); -- get byte to convert
         result (index) := chars (b / base + 1); -- convert hi bits
         result (index + 1) := chars (b mod base + 1); -- convert lo bits
         index := index + 2;

         -- insert dashes
         if i = 3 or else i = 5 or else i = 7 or else i = 9 then
            result (index) := '-';
            index := index + 1;
         end if;
      end loop;
      return result;
   end To_String;
end AdaID;
