-- File: test.adb
-- Description: Test suite for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Run;
with AUnit.Reporter.Text;
with AdaID_Tests;

procedure Test is
	function Suite return Access_Test_Suite is
		Result : constant Access_Test_Suite := new Test_Suite;
                Test : constant AdaID_Tests.Access_UUID_Test := new AdaID_Tests.UUID_Test;
	begin
		Add_Test(Result, Test);
		return Result;
	end Suite;

	procedure Run is new AUnit.Run.Test_Runner(Suite);
    Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
	Run(Reporter);
end;
