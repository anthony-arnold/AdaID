-- File: adaid.adb
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

package body AdaID is

	
	-- Default "Constructor" for NIL UUID
	overriding procedure Initialize(This: in out UUID) is
	begin
		for i in 1 .. uuid_size loop
			This.data(i) := 0;
		end loop;
	end;
	
	-- Generate a randome UUID
	function Random return UUID is
		id : UUID;
	begin
		return id; --placeholder
	end;
	
	-- Determine if the UUID is NIL
	function IsNil(This: in UUID) return Boolean is
	begin
		for i in 1 .. uuid_size loop
			if This.data(i) /= 0 then
				return false;
			end if;
		end loop;
		return true;
	end IsNil; 
	
	--Get the UUID Version
	function GetVersion(This: in UUID) return VersionType is
		-- version type in octect 9
		b : constant Byte := This.data(6) and Byte(16#0F#);
	begin
		case b is
			when 16#10# => return Time_Based;
			when 16#20# => return DCE_Security;
			when 16#30# => return Name_Based_MD5;
			when 16#40# => return Random_Number_Based;
			when 16#50# => return Name_Based_SHA1;
			when others => return Unknown;
		end case;
	end GetVersion;
	
	
	--Test for equality
	function "="(Left, Right: in UUID) return Boolean is
	begin
		for i in 1 .. uuid_size loop
			if Left.data(i) /= Right.data(i) then
				return false;
			end if;
		end loop;
		return true;
	end "=";
	
	
	--Get the hash value for the UUID
	function GetHashValue(This: in UUID) return SizeType is
		seed : SizeType := 0;
	begin
		for i in 1 .. uuid_size loop
			seed := seed or
					(
						SizeType(This.data(i)) 
						+ 16#9E3779B9#
						+ shift_left(seed, 6)
						+ shift_right(seed, 2)
					);
		end loop;
		return seed;
	end GetHashValue;
end AdaID;
