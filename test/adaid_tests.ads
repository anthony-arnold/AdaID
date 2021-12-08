-- File: adaid_tests.ads
-- Description: Test suite for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt
with AUnit; use AUnit;
with AUnit.Test_Cases;
package AdaID_Tests is

   type UUID_Test is new AUnit.Test_Cases.Test_Case with null record;
   type Access_UUID_Test is access UUID_Test;

   -- Register the tests to run
   procedure Register_Tests(T : in out UUID_Test);

   -- Register the test name
   function Name (T : UUID_Test) return Message_String;

private
   -- Test routines
   procedure Initialization(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure SetNil(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure GetVersion(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure GetVariant(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Equals(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure GetHashCode(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure RandomNotNil(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure RandomUnique(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure FromNameNotNil(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure FromNameUnique(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure FromNameEqual(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure ToString(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure FromStringNil(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure FromStringEqual(T : in out AUnit.Test_Cases.Test_Case'Class);
   procedure FromBadString(T : in out AUnit.Test_Cases.Test_Case'Class);
end AdaID_Tests;
