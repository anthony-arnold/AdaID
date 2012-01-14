-- File: adaid.ads
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with Ada.Finalization;
with Interfaces; use Interfaces; -- for Unsigned_n
package AdaID is
	-- Size type is unsigned 
	subtype SizeType is Unsigned_32 range 0 .. Unsigned_32'last;
	uuid_size: constant Integer := 16;
	
	--Hash Type
	subtype HashType is SizeType;
	
	-- Byte Type (2^8)
	subtype Byte is Unsigned_8;

	-- Byte Array
	type ByteArray is array (0 .. 15) of Byte;
	
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
	function IsNil(This: in UUID) return Boolean;
	
	--Get the UUID Version
	function GetVersion(This: in UUID) return VersionType;
	
	--Get the UUID Variant
	function GetVariant(This: in UUID) return VariantType;
	
	--Test for equality
	function "="(Left, Right: in UUID) return Boolean;
	
	--Get the hash value for the UUID
	function GetHashValue(This: in UUID) return SizeType;
	
	
	-------------------- GENERATORS -----------------------
	
	--Generate a random UUID
	function Random return UUID;
	
	--Generate a UUID based on a name
	function FromName(name : in String) return UUID;
	
private
	--Default "constructor", initializes to NIL
	overriding procedure Initialize (This: in out UUID);
end AdaID;
