LIB=./lib/libadaid.so
TEST=./bin/test

#library
$(LIB): src/*.adb src/*.ads include/*.ads
	gnatmake -Padaid.gpr
	

#test executable
$(TEST): $(LIB) include/*.ads test/*.adb test/*.ads
	gnatmake -Ptest.gpr

#run tests
test: $(TEST)
	@export LD_LIBRARY_PATH=./lib;\
	$(TEST)

#misc
.PHONY: all

all: $(LIB) $(TEST)
	
.PHONY: clean
.PHONY: cleanall
clean:
	rm -f obj/* 2> /dev/null
cleanall: clean
	rm -f bin/* lib/* 2> /dev/null

