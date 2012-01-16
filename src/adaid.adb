-- File: adaid.adb
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with SHA.Process_Data; 
with Ada.Numerics.Discrete_Random;
package body AdaID is

	-- For RNG
	package RNG is new Ada.Numerics.Discrete_Random(Result_Subtype => Unsigned_64);
	generator: RNG.Generator;
	generator_is_set: Boolean := false;
	
	-- Default "Constructor" for NIL UUID
	overriding procedure Initialize(This: in out UUID) is
	begin
		for i in ByteArray'Range loop
			This.data(i) := 0;
		end loop;
	end;
	
	-- Determine if the UUID is NIL
	function Is_Nil(This: in UUID) return Boolean is
	begin
		for i in ByteArray'Range loop
			if This.data(i) /= 0 then
				return false;
			end if;
		end loop;
		return true;
	end Is_Nil; 
	
	--Get the UUID Version
	function Get_Version(This: in UUID) return VersionType is
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
	end Get_Version;
	
	--Get the UUID Variant
	function Get_Variant(This: in UUID) return VariantType is
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
	end Get_Variant;
	
	--Test for equality
	function "="(Left, Right: in UUID) return Boolean is
	begin
		for i in ByteArray'Range loop
			if Left.data(i) /= Right.data(i) then
				return false;
			end if;
		end loop;
		return true;
	end "=";
	
	
	--Get the hash value for the UUID
	function Get_Hash_Value(This: in UUID) return SizeType is
		seed : SizeType := 0;
	begin
		for i in ByteArray'Range loop
			seed := seed xor
					(
						SizeType(This.data(i)) 
						+ 16#9E3779B9#
						+ shift_left(seed, 6)
						+ shift_right(seed, 2)
					);
		end loop;
		return seed;
	end Get_Hash_Value;
	
	--Convert the UUID to a string
	function To_String(This: in UUID) return String is
		result : String(1 .. 36);
		index : Integer := 1;
		base : constant Integer := 2 ** 4;
		chars : constant String(1 .. base) := "0123456789ABCDEF";
		b : Integer;
	begin
		for i in ByteArray'Range loop
			b := Integer(This.data(i));
			result(index) := chars(b / base + 1);
			result(index + 1) := chars(b mod base + 1);
			index := index + 2;
			
			if i = 3 or i = 5 or i = 7 or i = 9 then
				result(index) := '-';
				index := index + 1;
			end if;
		end loop;
		return result;
	end To_String;
	
	--=============== GENERATORS ===============--
	
	--Reset a UUID to Nil
	procedure Nil(id : in out UUID) is
	begin
		Initialize(id);
	end Nil;
	
	-- Generate a randome UUID
	procedure Random(id : in out UUID) is
		rand : Unsigned_64;
		x : Integer := 0;
	begin
		-- Set up the generator first time
		if not generator_is_set then
			RNG.Reset(generator);
			generator_is_set := true;
		end if;
		
		rand := RNG.Random(generator);
		
		for i in ByteArray'Range loop
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
	end;
	
	
	--Generate a UUID based on a name
	procedure From_Name(namespace: in UUID; name: in String; id: in out UUID) is
		use SHA.Process_Data;
		
		c : Context;
		d : SHA.Digest;
	begin
		Initialize(c);
		
		-- Start SHA1 hashing with namespace uuid
		for i in namespace.data'Range loop
			Add(SHA.Process_Data.Byte(namespace.data(i)), c);
		end loop;
		
		-- Continuing hashing the actual name
		for i in name'Range loop
			Add(SHA.Process_Data.Byte(Character'Pos(name(i))), c);
		end loop;
		
		--Get the digest
		Finalize(d, c);
	
		-- Now make the UUID from the hash
		for i in 0 .. 3 loop
			id.data(i*4+0) := Byte(shift_right(d(i), 24) and 16#FF#);
			id.data(i*4+1) := Byte(shift_right(d(i), 16) and 16#FF#);
			id.data(i*4+2) := Byte(shift_right(d(i),  8) and 16#FF#);
			id.data(i*4+3) := Byte(shift_right(d(i),  0) and 16#FF#);
		end loop;
		
		-- set variant
		id.data(8) := (id.data(8) and 16#BF#) or 16#80#;
		
		--set version
		id.data(6) := (id.data(6) and 16#5F#) or 16#50#;
	end From_Name;
	
	
	-- Generate a UUID from a string.
	-- This is not so much generation, but reconstruction
	procedure From_String(str : in String; id : in out UUID) is
		delim: constant Character := '-';
		open : constant Character := '{';
		close : constant Character := '}';
		
		-- expect dashes if str is 36 or 38 in length
		dashed: constant Boolean := str'Length = 36 or str'Length = 38;
		braced: constant Boolean := str(str'First) = open and 
									str(str'Last) = close;
		
		idx : Integer := 0;
		start, rel : Integer;
		
	begin
		
		-- Check that length is valid
		if not dashed and str'Length /= 32 and str'Length /= 34 then
			raise Invalid_String;
		end if;
		
		-- Check that brace are valid
		start := str'First;
		if not braced and
			(str(str'First) = open or str(str'Last) = close) then
			raise Invalid_String; -- only one brace present
		elsif braced then
			start := str'First + 1;
		end if;
		
		idx := start;
		
		-- Grab each pair and stuff into byte
		for i in ByteArray'Range loop
			rel := idx - start;
			if dashed and (rel = 8 or rel = 13 or rel = 18 or rel = 23) then
				if str(idx) /= delim then
					raise Invalid_String; -- expected '-'
				end if;
				idx := idx + 1;
			end if;
			-- Convert to byte
			id.data(i) := Byte'Value("16#" & str(idx .. idx+1) & "#");
			idx := idx + 2;
		end loop;
	end From_String;
	
end AdaID;
