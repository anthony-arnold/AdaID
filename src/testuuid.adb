-- File: testuuid.adb
-- Description: Test program for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AdaID;
with Ada.Text_IO; 

procedure testuuid is
	id: AdaID.UUID;
begin
	Ada.Text_IO.Put_Line("Testing IsNil");
	if not AdaID.IsNil(id) then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	Ada.Text_IO.Put_Line("Testing Complete");
end;

