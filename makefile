LIB=./lib/libadaid.so
DEBUG=./lib/libadaid.a
TEST=./bin/test

#library
$(LIB): src/*.adb src/*.ads include/*.ads
	gnatmake -Padaid.gpr

#debugging
.PHONY: debug

debug: $(DEBUG)

$(DEBUG): src/*.adb src/*.ads include/*.ads
	gnatmake -Padaid_debug.gpr

#test executable
$(TEST): $(LIB) include/*.ads test/*.adb test/*.ads
	gnatmake -Ptest.gpr

#run tests
test: $(TEST)
	$(TEST)

#misc
.PHONY: all

all: $(LIB) $(TEST)
	
.PHONY: clean
.PHONY: cleanall
clean:
	rm -f obj/*.* obj/test/*.* obj/debug/*.* ali/*.* ali/debug/*.* 2> /dev/null
cleanall: clean
	rm -f bin/* lib/* 2> /dev/null

