#AdaID#

**AdaID** is a simple Ada library for generating [UUIDs](http://en.wikipedia.org/wiki/Universally_unique_identifier). Supported operations include:

 - Generating random UUIDs
 - Generating name-based UUIDs
 - Restoring UUIDs from a string representation (Not yet implemented).
 - Converting a UUID to a string

Included is [Jonh Halleck's NIST-validated implementation of SHA1](http://home.utah.edu/~nahaj/ada/sha/) (distrubted under the GNU GPLv3).

To use the library, ensure that the latest version of [GNAT](http://www.gnu.org/software/gnat/) is installed. Then run `make`, followed by an optional `make test`. Then, use the `src/adaid.ads` file and link your project with `libadaid.a`.

Here's an example usage:

	with AdaID; use AdaID;
	with Ada.Text_IO; use Ada.Text_IO;
    procedure Print_Random_UUID is
    	id : UUID;
    begin
    	Random(id);
    	Put_Line(To_String(id));
    end Print_Random_UUID;

Future improvements include:

 - Moving the test program to a proper unit test framework
 - Including an install option in the build process
 - General make file improvements

