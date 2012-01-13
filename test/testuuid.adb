-- File: testuuid.adb
-- Description: Test program for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AdaID;
with Ada.Text_IO; 

use AdaID;
with Interfaces; use Interfaces;
procedure testuuid is
	id, id2: AdaID.UUID;
begin
	Ada.Text_IO.Put("Testing IsNil: ");
	if not AdaID.IsNil(id) then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	
	Ada.Text_IO.Put("Testing GetVersion: ");
	if AdaID.GetVersion(id) /= Unknown then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	
	
	Ada.Text_IO.Put("Testing =: ");
	if id /= id2 then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	
	
	
	Ada.Text_IO.Put("Testing GetHashValue: ");
	if GetHashValue(id) /= GetHashValue(id2) then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	Ada.Text_IO.Put_Line("Testing Complete");
end;

