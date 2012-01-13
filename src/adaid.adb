-- File: adaid.adb
-- Description: A UUID type for Ada
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

package body AdaID is


	function IsNil(This: in UUID) return Boolean is
	begin
		for i in 1 .. uuid_size loop
			if This.data(i) /= 0 then
				return false;
			end if;
		end loop;
		return true;
	end IsNil; 
	
	
	overriding procedure Initialize(This: in out UUID) is
	begin
		for i in 1 .. uuid_size loop
			This.data(i) := 0;
		end loop;
	end;
	
	
	function Generate return UUID is
		id : UUID;
	begin
		return id;
	end;
end AdaID;
