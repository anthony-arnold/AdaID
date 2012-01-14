-- File: adaid.adb
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with Ada.Numerics.Discrete_Random;
with SHA.Process_Data; use SHA.Process_Data;
package body AdaID is

	-- For RNG
	package RNG is new Ada.Numerics.Discrete_Random(Result_Subtype => Unsigned_64);
	generator: RNG.Generator;
	generator_is_set: Boolean := false;
	
	-- Default "Constructor" for NIL UUID
	overriding procedure Initialize(This: in out UUID) is
	begin
		for i in 0 .. uuid_size-1 loop
			This.data(i) := 0;
		end loop;
	end;
	
	-- Determine if the UUID is NIL
	function IsNil(This: in UUID) return Boolean is
	begin
		for i in 0 .. uuid_size-1 loop
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
	
	--Get the UUID Variant
	function GetVariant(This: in UUID) return VariantType is
		-- variant type in octet 7
		b : constant Byte := This.data(8);
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
	end GetVariant;
	
	--Test for equality
	function "="(Left, Right: in UUID) return Boolean is
	begin
		for i in 0 .. uuid_size-1 loop
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
		for i in 0 .. uuid_size-1 loop
			seed := seed xor
					(
						SizeType(This.data(i)) 
						+ 16#9E3779B9#
						+ shift_left(seed, 6)
						+ shift_right(seed, 2)
					);
		end loop;
		return seed;
	end GetHashValue;
	
	--=============== GENERATORS ===============--
	
	-- Generate a randome UUID
	function Random return UUID is
		id : UUID;
		rand : Unsigned_64;
		x : Integer := 0;
	begin
		-- Set up the generator first time
		if not generator_is_set then
			RNG.Reset(generator);
			generator_is_set := true;
		end if;
		
		rand := RNG.Random(generator);
		
		for i in 0 .. uuid_size-1 loop
			if x = Unsigned_64'Size then
				x := 0;
				rand := RNG.Random(generator);
			end if;
			id.data(i) := Byte(shift_right(rand, x * 8) and 16#FF#);
		end loop;
		
		-- Set the variant
		id.data(8) := (id.data(8) and 16#BF#) or 16#80#;
		
		--Set the version to random-number-based
		id.data(6) := (id.data(6) and 16#4F#) or 16#40#;
		
		return id;
	end;
	
	
	--Generate a UUID based on a name
	function FromName(name : in String) return UUID is
		id : UUID;
		d : constant SHA.Digest := Digest_A_String(name);
	begin
		for i in 0 .. 3 loop
			
			id.data(i*4+0) := Unsigned_8(shift_right(d(i), 24) and 16#FF#);
			id.data(i*4+1) := Unsigned_8(shift_right(d(i), 16) and 16#FF#);
			id.data(i*4+2) := Unsigned_8(shift_right(d(i),  8) and 16#FF#);
			id.data(i*4+3) := Unsigned_8(shift_right(d(i),  0) and 16#FF#);
		end loop;
		
		-- set variant
		id.data(8) := (id.data(8) and 16#BF#) or 16#80#;
		
		--set version
		id.data(6) := (id.data(6) and 16#5F#) or 16#50#;
		
		return id;
	end;
	
end AdaID;
