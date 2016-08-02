-------------------------------------------------------------------------------
-- File: adaid-generate.ads
-- Description: UUID Generation
-- Author: Anthony Arnold
-- License: Simplified BSD License (see LICENSE)
-------------------------------------------------------------------------------

-- AdaID.Generate defines the functions required to generate UUIDs using
-- different standard methods.
package AdaID.Generate is

   Invalid_String : exception;
   -- Thrown when invalid strings are passed to From_String


   procedure Nil (id : in out UUID);
   -- Set a UUID to Nil


   procedure Random (id : in out UUID);
   -- Generate a random UUID

   procedure From_Name
     (namespace : in UUID;
      name      : in String;
      id        : in out UUID);
   -- Generate a UUID based on a name

   procedure From_String (str : in String; id : in out UUID);
   -- Generate a UUID from a string.
   -- This is not so much generation, but reconstruction

end AdaID.Generate;
