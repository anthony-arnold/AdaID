-- File: testuuid.adb
-- Description: Test suite for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AdaID; use AdaID;
with AdaID.Generate; use AdaID.Generate;
with Interfaces; use Interfaces;
with AUnit.Assertions; use AUnit.Assertions;
package body AdaID_Tests is


   -- Register the tests to run
   procedure Register_Tests(T : in out UUID_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine(T, Initialization'Access, "Initialization");
      Register_Routine(T, SetNil'Access, "Set to Nil");
      Register_Routine(T, GetVersion'Access, "Get Version");
      Register_Routine(T, GetVariant'Access, "Get Variant");
      Register_Routine(T, Equals'Access, "Check Equality");
      Register_Routine(T, GetHashCode'Access, "Get Hash Code");
      Register_Routine(T, RandomNotNil'Access, "Random UUID is not Nil");
      Register_Routine(T, RandomUnique'Access, "Random UUIDs are unique");
      Register_Routine(T, FromNameNotNil'Access, "UUID from a name is not Nil");
      Register_Routine(T, FromNameUnique'Access, "UUID from name is unique");
      Register_Routine(T, FromNameEqual'Access, "UUIDs from same name are equal");
      Register_Routine(T, ToString'Access, "Convert to String");
      Register_Routine(T, FromStringNil'Access, "UUID from Nil String is Nil");
      Register_Routine(T, FromStringEqual'Access, "Make same UUID from common formats");
      Register_Routine(T, FromBadString'Access, "Expecting Invalid_String");
   end Register_Tests;

   -- Register the test name
   function Name (T : UUID_Test) return Message_String is
   begin
      return AUnit.Format ("AdaID Tests");
   end Name;

   -- =================== Test routines ===================== --

   -- Test that a UUID is Nil on initialize
   procedure Initialization(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
   begin
      Assert(Is_Nil(id), "UUID not initialized to nil");
   end Initialization;

   -- Test that Set_Nil sets a UUID to Nil
   procedure SetNil(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
   begin
      Nil(id);
      Assert(Is_Nil(id), "UUID not set to nil");
   end SetNil;

   -- Test that Get_Version returns the correct version
   procedure GetVersion(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
   begin
      Assert(Get_Version(id) = Unknown, "Wrong version returned");
   end GetVersion;

   -- Test that Get_Variant returns the correct variant
   procedure GetVariant(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
   begin
      Assert(Get_Variant(id) = NCS, "Wrong variant returned");
   end GetVariant;

   -- Test the equals operator
   procedure Equals(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2 : UUID;
   begin
      Assert(id1 = id2, "UUIDs not considered equal");
   end Equals;

   -- Test the Get_Hash_Value function, and ensure that
   --  two equal UUIDs have the same Hash code.
   procedure GetHashCode(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2 : UUID;
   begin
      Assert(Get_Hash_Value(id1) = Get_Hash_Value(id2),
             "Equal IDs don't have equal hash codes");
   end GetHashCode;

   -- Ensure the Random generator does not make Nil
   procedure RandomNotNil(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
   begin
      Random(id);
      Assert(not Is_Nil(id), "Random UUID is Nil");
   end RandomNotNil;

   -- Ensure the random generator makes unique IDs
   procedure RandomUnique(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2 : UUID;
   begin
      Random(id1);
      Random(id2);
      Assert(id1 /= id2, "Two UUIDs are not unique");
   end RandomUnique;

   -- Ensure the name-based generator does not make Nils
   procedure FromNameNotNil(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2 : UUID;
   begin
      Random(id1);
      From_Name(id1, "Test_String", id2);
      Assert(not Is_Nil(id2), "UUID from name turned out Nil");
   end FromNameNotNil;

   procedure FromNameUnique(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2 : UUID;
   begin
      Random(id1);
      From_Name(id1, "Test_String", id2);
      Assert(id1 /= id2, "UUIDs are not unique");
   end FromNameUnique;

   procedure FromNameEqual(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2, id3 : UUID;
   begin
      Random(id1);
      From_Name(id1, "Test_String", id2);
      Assert(not Is_Nil(id2), "UUID from name turned out Nil");
      Assert(id1 /= id2, "UUIDs are not unique");

      From_Name(id1, "Test_String", id3);
      Assert(id2 = id3, "Equal named UUIDs not Equal");
   end FromNameEqual;

   procedure FromStringNil(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
      s : constant String := "00000000-0000-0000-0000-000000000000";
   begin
      From_String(s, id);
      Assert(Is_Nil(id), "Wrong UUID from Nil string");
   end FromStringNil;

   procedure FromStringEqual(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id1, id2 : UUID;
      w : constant String := "12345678-90AB-CDEF-1234-567890ABCDEF";
      x : constant String := "{12345678-90AB-CDEF-1234-567890ABCDEF}";
      y : constant String := "{1234567890ABCDEF1234567890ABCDEF}";
      z : constant String := "1234567890ABCDEF1234567890ABCDEF";
   begin
      From_String(w, id1);
      From_String(x, id2);
      Assert(id1 = id2, "Equal UUID strings generated distinct UUIDs");
      From_String(y, id2);
      Assert(id1 = id2, "Equal UUID strings generated distinct UUIDs");
      From_String(z, id2);
      Assert(id1 = id2, "Equal UUID strings generated distinct UUIDs");
   end FromStringEqual;



   procedure ToString(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
      s : constant String := To_String(id);
   begin
      Assert(s = "00000000-0000-0000-0000-000000000000", "Incorrect UUID String");
   end ToString;

   procedure FromBadString(T : in out AUnit.Test_Cases.Test_Case'Class) is
      id : UUID;
      s : constant String := "not-a-uuid";
   begin
      From_String(s, id);
      Assert(false, "Should have raised Invalid_String");
   exception
      when Invalid_String => null;
   end FromBadString;


end AdaID_Tests;
