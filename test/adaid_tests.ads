-- File: adaid_tests.ads
-- Description: Test suite for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package AdaID_Tests is

	type UUID_Test is new Test_Case with null record;

	-- Register the tests to run
	procedure Register_Tests(T : in out UUID_Test);
	
	-- Register the test name
	function Name (T : UUID_Test) return String_Access;

private	
	-- Test routines
	procedure Initialization(T : in out Test_Case'Class);
	procedure SetNil(T : in out Test_Case'Class);
	procedure GetVersion(T : in out Test_Case'Class);
	procedure GetVariant(T : in out Test_Case'Class);
	procedure Equals(T : in out Test_Case'Class);
	procedure GetHashCode(T : in out Test_Case'Class);
	procedure Random(T : in out Test_Case'Class);
	procedure FromName(T : in out Test_Case'Class);
	procedure ToString(T : in out Test_Case'Class);
	procedure FromString(T : in out Test_Case'Class);
	--procedure FromBadString(T : in out Test_Case'Class);
end AdaID_Tests;
