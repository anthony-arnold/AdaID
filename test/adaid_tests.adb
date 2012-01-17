-- File: testuuid.adb
-- Description: Test suite for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AdaID; use AdaID;
with AdaID.Generate; use AdaID.Generate;
with Interfaces; use Interfaces;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions; use AUnit.Assertions;
package body AdaID_Tests is


	-- Register the tests to run
	procedure Register_Tests(T : in out UUID_Test) is
	begin
		Register_Routine(T, Initialization'Access, "Initialization");
		Register_Routine(T, SetNil'Access, "Set to Nil");
		Register_Routine(T, GetVersion'Access, "Get Version");
		Register_Routine(T, GetVariant'Access, "Get Variant");
		Register_Routine(T, Equals'Access, "Check Equality");
		Register_Routine(T, GetHashCode'Access, "Get Hash Code");
		Register_Routine(T, Random'Access, "Get Random UUID");
		Register_Routine(T, FromName'Access, "Get UUID From Name");
		Register_Routine(T, ToString'Access, "Convert to String");
		Register_Routine(T, FromString'Access, "Restore From String");
		--Register_Routine(T, FromBadString'Access, "Expecting Invalid_String");
	end Register_Tests;
		-- Register the test name
	function Name (T : UUID_Test) return String_Access is
	begin
		return new String'("AdaID Tests");
	end Name;

	-- Test routines
	procedure Initialization(T : in out Test_Case'Class) is
		id : UUID;
	begin
		Assert(Is_Nil(id), "UUID not initialized to nil");
	end Initialization;

	procedure SetNil(T : in out Test_Case'Class) is
		id : UUID;
	begin
		Nil(id);
		Assert(Is_Nil(id), "UUID not set to nil");
	end SetNil;

	procedure GetVersion(T : in out Test_Case'Class) is
		id : UUID;
	begin
		Assert(Get_Version(id) = Unknown, "Wrong version returned");
	end GetVersion;

	procedure GetVariant(T : in out Test_Case'Class) is
		id : UUID;
	begin
		Assert(Get_Variant(id) = NCS, "Wrong variant returned");
	end GetVariant;

	procedure Equals(T : in out Test_Case'Class) is
		id1, id2 : UUID;
	begin
		Assert(id1 = id2, "UUIDs not considered equal");
	end Equals;

	procedure GetHashCode(T : in out Test_Case'Class) is
		id1, id2 : UUID;
	begin
		Assert(Get_Hash_Value(id1) = Get_Hash_Value(id2),
			"Equal IDs don't have equal hash codes");
	end GetHashCode;

	procedure Random(T : in out Test_Case'Class) is
		id1, id2 : UUID;
	begin
		Random(id1);
		Assert(not Is_Nil(id1), "Random UUID turned out Nil");
		Random(id2);
		Assert(id1 /= id2, "Two subsequent random UUIDs are equal");
	end Random;

	procedure FromName(T : in out Test_Case'Class) is
		id1, id2 : UUID;
	begin
		Random(id1);
		From_Name(id1, "Test_String", id2);
		Assert(not Is_Nil(id2), "UUID from name turned out Nil");
		Assert(id1 /= id2, "UUIDs are not unique");
	end FromName;

	procedure ToString(T : in out Test_Case'Class) is
		id : UUID;
		s : constant String := To_String(id);
	begin
		Assert(s = "00000000-0000-0000-0000-000000000000", "Incorrect UUID String");
	end ToString;

	procedure FromString(T : in out Test_Case'Class) is
		id1, id2 : UUID;
		w : constant String := "00000000-0000-0000-0000-000000000000";
		x : constant String := "{00000000-0000-0000-0000-000000000000}";
		y : constant String := "{00000000000000000000000000000000}";
		z : constant String := "00000000000000000000000000000000";
	begin
		From_String(w, id1);
		Assert(Is_Nil(id1), "Wrong UUID from Nil string");
		From_String(x, id1);
		Assert(Is_Nil(id1), "Wrong UUID from Nil string");
		From_String(y, id1);
		Assert(Is_Nil(id1), "Wrong UUID from Nil string");
		From_String(z, id1);
		Assert(Is_Nil(id1), "Wrong UUID from Nil string");

		Random(id2);
		From_String(To_String(id2), id1);
		Assert(id1 = id2, "Wrong UUID from random string");
	end FromString;

	--procedure FromBadString(T : in out Test_Case'Class) is
	--	id : UUID;
	--	s : constant String := "not-a-uuid";
	--begin
	--	begin
	--		From_String(s, id);
	--		Fail("Invalid_String expected");
	--	exception
	--		when Invalid_String =>
	--			null; -- expected
	--	end;
	--end FromBadString;

end AdaID_Tests;

