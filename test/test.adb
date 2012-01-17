-- File: test.adb
-- Description: Test suite for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Runner;

with AdaID_Tests;

procedure Test is
	function Suite return Access_Test_Suite is
		Result : Access_Test_Suite := new Test_Suite;
	begin
		Add_Test(Result, new AdaID_Tests.UUID_Test);
		return Result;
	end Suite;
	
	procedure Run is new AUnit.Test_Runner(Suite);
begin
	Run;
end;
