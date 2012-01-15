-- File: testuuid.adb
-- Description: Test program for AdaID
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

with AdaID;
with Ada.Text_IO; 

use AdaID;

with Interfaces; use Interfaces;
procedure testuuid is
	id, id2, id3: AdaID.UUID;
begin
	Ada.Text_IO.Put("Testing IsNil: ");
	Nil(id);
	if not IsNil(id) or not IsNil(id2) then
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
	
	
	
	Ada.Text_IO.Put("Testing GetVariant: ");
	if AdaID.GetVariant(id) /= NCS then
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
	
	
	Ada.Text_IO.Put("Testing Random: ");
	Random(id);
	Random(id2);
	Random(id3);
	if id = id2 or id2 = id3 or id = id3 then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	
	Ada.Text_IO.Put("Testing FromName: ");
	FromName(id3, "Hello, World!", id);
	FromName(id3, "Hello, Joe!", id2);
	if id = id2 then
		Ada.Text_IO.Put_Line("Failed");
	else
		Ada.Text_IO.Put_Line("Passed");
	end if;
	
	
	
	Ada.Text_IO.Put("Testing ToString: ");
	FromName(id3, "UUID", id);
	Ada.Text_IO.Put_Line(To_String(id));
	--if  /= "6ba7b810-9dad-11d1-80b4-00c04fd430c8" then
	--	Ada.Text_IO.Put_Line("Failed");
	--else
	--	Ada.Text_IO.Put_Line("Passed");
	--end if;
	
	Ada.Text_IO.Put_Line("Testing Complete");
	
end;

