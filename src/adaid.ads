-- File: adaid.ads
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with Ada.Finalization;
with Interfaces; use Interfaces; -- for Unsigned_n
package AdaID is

	-- Size type is unsigned 
	subtype SizeType is Unsigned_32;
	uuid_size: constant Integer := 16; -- This many bytes in a UUID
	
	--Hash Type
	subtype HashType is SizeType; -- The 'hash' of a UUID
	
	-- Byte Type (2^8)
	type Byte is mod 2 ** 8; -- 8-bit bytes

	-- Byte Array for UUID data storage
	type ByteArray is array (0 .. uuid_size-1) of Byte; 
	
	-- Some UUID Enums
	type VersionType is (
		Unknown,
		Time_Based,
		DCE_Security,
		Name_Based_MD5,
		Random_Number_Based,
		Name_Based_SHA1
	);
	
	type VariantType is (
		NCS,
		RFC_4122,
		Microsoft,
		Future
	);

	-- The main type for the package
	type UUID is new Ada.Finalization.Controlled with 
	record
		data: ByteArray;
	end record;
	
	--Determine if UUID is NIL
	function Is_Nil(This: in UUID) return Boolean;
	
	--Get the UUID Version
	function Get_Version(This: in UUID) return VersionType;
	
	--Get the UUID Variant
	function Get_Variant(This: in UUID) return VariantType;
	
	--Test for equality
	function "="(Left, Right: in UUID) return Boolean;
	
	--Get the hash value for the UUID
	function Get_Hash_Value(This: in UUID) return SizeType;
	
	--Convert the UUID to a string
	function To_String(This: in UUID) return String;
	function To_Wide_String(This: in UUID) return Wide_String;
	
	-------------------- GENERATORS -----------------------
	-- Set a UUID to Nil
	procedure Nil(id : in out UUID);
	
	-- Generate a random UUID
	procedure Random(id : in out UUID);
	
	-- Generate a UUID based on a name
	procedure From_Name(namespace: in UUID; name: in String; id: in out UUID);
	
	-- Generate a UUID from a string.
	-- This is not so much generation, but reconstruction
	--procedure From_String(str : in String; id : in out UUID);
	--procedure From_Wide_String(str : in Wide_String; id : in out UUID);
	
private
	--Default "constructor", initializes to NIL
	overriding procedure Initialize (This: in out UUID);
end AdaID;
