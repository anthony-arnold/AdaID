#AdaID#

**What is it?**
*AdaID* is a simple Ada library for generating [UUIDs](http://en.wikipedia.org/wiki/Universally_unique_identifier). Supported operations include:

 - Generating random UUIDs
 - Generating name-based UUIDs
 - Restoring UUIDs from a string representation.
 - Converting a UUID to a string

**What are the dependencies?**
All of the code is self-contained, but the build system assumes GNAT is present.

If you want to run the unit test suite, you'll need [AUnit](http://libre.adacore.com/libre/tools/aunit/) installed. If you don't have it, and you don't care about running the tests (and you *should*), then just avoid running `make test`.


**How do I build it?**
To build the library: run `make`, optionally followed by `make test`, and finally `make install`. The library should now be installed on your system.

**OK, how is it used?**
Here's an example usage:

	-- file: print_random_uuid.adb
	with AdaID; use AdaID;
	with Ada.Text_IO; use Ada.Text_IO;
    procedure Print_Random_UUID is
    	id : UUID;
    begin
    	Random(id);
    	Put_Line(To_String(id));
    end Print_Random_UUID;

To compile, create a [gnat project file](http://www.adacore.com/wp-content/files/auto_update/gnat-unw-docs/html/gnat_ugn_12.html) to build the example:

	with "adaid.gpr";
	project Print_Random_UUID is
		for Main use ("print_random_uuid.adb");
		for Languages use ("Ada");
	end Print_Random_UUID;

And build:

	gnatmake -Pprint_random_uuid.gpr

**What's next?**
Future improvements include:

 - General make file improvements
 - Library documentation
