-------------------------------------------------------------------------------
-- File: adaid.ads
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: Simplified BSD License (see LICENSE)
-------------------------------------------------------------------------------

with Ada.Finalization;
with Interfaces; use Interfaces; -- for Unsigned_n

-- AdaID defines the types and accessor/miscellaneous functions for
-- the UUID type.
package AdaID is

   uuid_size : constant Integer := 16;
   -- This many bytes in a UUID

   subtype HashType is Unsigned_32;
   -- Represents a "Hash Code" of a UUID

   type Byte is mod 2 ** 8;
   -- Byte Type (2^8)

   type ByteArray is array (0 .. uuid_size - 1) of Byte;
   -- Byte Array for UUID data storage

   type VersionType is (
                        Unknown,
                        Time_Based,
                        DCE_Security,
                        Name_Based_MD5,
                        Random_Number_Based,
                        Name_Based_SHA1
                       );
   -- The Version of the UUID

   type VariantType is (
                        NCS,
                        RFC_4122,
                        Microsoft,
                        Future
                       );
   -- The UUID Variant


   type UUID is new Ada.Finalization.Controlled with
      record
         data : ByteArray;
      end record;
   -- The main type for the package

   function Is_Nil (This : in UUID) return Boolean;
   -- Determine if UUID is NIL (All Zeros)

   function Get_Version (This : in UUID) return VersionType;
   -- Get the UUID Version

   function Get_Variant (This : in UUID) return VariantType;
   -- Get the UUID Variant

   function "="(Left, Right : in UUID) return Boolean;
   -- Test for equality between Left and Right

   function Get_Hash_Value (This : in UUID) return HashType;
   -- Get the hash code for the UUID

   function To_String (This : in UUID) return String;
   -- Convert the UUID to a common string representation

private
   overriding procedure Initialize (This : in out UUID);
   -- Default "constructor", initializes to NIL
end AdaID;
