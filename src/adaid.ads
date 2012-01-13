-- File: adaid.ads
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with Ada.Finalization;
with Interfaces; use Interfaces; -- for Byte
package AdaID is
	-- Size type is unsigned 
	subtype SizeType is Unsigned_32 range 0 .. Unsigned_32'last;
	uuid_size: constant SizeType := 16;
	
	-- Byte Type (2^8)
	subtype Byte is Unsigned_8;

	-- Byte Array
	type ByteArray is array (1 .. uuid_size) of Byte;
	
	-- Some UUID Enums
	type VersionType is (
		Unknown,
		Time_Based,
		DCE_Security,
		Name_Based_MD5,
		Random_Number_Based,
		Name_Based_SHA1
	);

	-- The main type for the package
	type UUID is new Ada.Finalization.Controlled with 
	record
		data: ByteArray;
	end record;
	
	
	--Generate a random UUID
	function Random return UUID;
	
	--Determine if UUID is NIL
	function IsNil(This: in UUID) return Boolean;
	
	--Get the UUID Version
	function GetVersion(This: in UUID) return VersionType;
	
	--Test for equality
	function "="(Left, Right: in UUID) return Boolean;
	
	--Get the hash value for the UUID
	function GetHashValue(This: in UUID) return SizeType;
	
private
	--Default "constructor", initializes to NIL
	overriding procedure Initialize (This: in out UUID);
end AdaID;
