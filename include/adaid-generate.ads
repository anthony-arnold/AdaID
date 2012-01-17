-- File: adaid-generate.ads
-- Description: UUID Generation
-- Author: Anthony Arnold
-- License: http://www.gnu.org/licenses/gpl.txt

package AdaID.Generate is
	
	-- Exception for string parsing
	Invalid_String : exception;
	
	-- Set a UUID to Nil
	procedure Nil(id : in out UUID);
	
	-- Generate a random UUID
	procedure Random(id : in out UUID);
	
	-- Generate a UUID based on a name
	procedure From_Name(namespace: in UUID; name: in String; id: in out UUID);
	
	-- Generate a UUID from a string.
	-- This is not so much generation, but reconstruction
	procedure From_String(str : in String; id : in out UUID);
	
end AdaID.Generate;

