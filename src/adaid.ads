-- File: adaid.ads
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with Ada.Finalization;
package AdaID is
	-- Size type is unsigned 
	subtype SizeType is Integer range 0 .. Integer'last;
	uuid_size: constant SizeType := 16;
	
	-- Byte is 2^8
	subtype ByteType is Integer range 0 .. 255;

	-- Byte Array
	type ByteArray is array (1 .. uuid_size) of ByteType;

	-- The main type for the package
	type UUID is new Ada.Finalization.Controlled with 
	record
		data: ByteArray;
	end record;
	
	--Determine if UUID is NIL
	function IsNil(This: in UUID) return Boolean;
	
	--Generate a random UUID
	function Generate return UUID; 
	
private
	--Default "constructor", initializes to NIL
	overriding procedure Initialize (This: in out UUID);
end AdaID;
