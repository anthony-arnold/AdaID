#globals
SRC=./src
BIN=./bin
CC=gcc
AR=ar
FLAGS=-Wall -gnat12 -g
LIB=./lib/libadaid.a
OBJS=$(BIN)/adaid.o $(BIN)/sha.o $(BIN)/sha-process_data.o $(BIN)/adaid-generate.o
BIND=gnatbind
LNK=gnatlink
TEST=./test/testuuid.adb
TESTALI=$(BIN)/testuuid.ali
TESTUUID=$(BIN)/testuuid
TESTOBJ=$(BIN)/testuuid.o

.PHONY: all

all: $(LIB) $(TESTUUID)

#build library
$(LIB): $(OBJS)
	$(AR) rvs $(LIB) $(OBJS)

#build object files
$(BIN)/adaid.o: $(SRC)/adaid.adb $(SRC)/adaid.ads
	$(CC) -c $(FLAGS) -I$(SRC) -I- $(SRC)/adaid.adb -o $(BIN)/adaid.o
	
$(BIN)/adaid-generate.o: $(SRC)/adaid-generate.adb $(SRC)/adaid-generate.ads
	$(CC) -c $(FLAGS) -I$(SRC) -I- $(SRC)/adaid-generate.adb -o $(BIN)/adaid-generate.o
	
$(BIN)/sha.o: $(SRC)/sha.ads
	$(CC) -c $(FLAGS) -I$(SRC) -I- $(SRC)/sha.ads -o $(BIN)/sha.o
	
$(BIN)/sha-process_data.o: $(SRC)/sha-process_data.adb $(SRC)/sha-process_data.ads
	$(CC) -c $(FLAGS) -I$(SRC) -I- $(SRC)/sha-process_data.adb -o $(BIN)/sha-process_data.o
	
#test stuff

$(TESTUUID): $(TEST) $(LIB)
	$(CC) -c $(FLAGS) -I$(SRC) -I- $(TEST) -o $(TESTOBJ);\
	$(BIND) -I$(SRC) -x $(TESTALI);\
	$(LNK) $(TESTALI) -o $(TESTUUID)
	
.PHONY: test

test: $(TESTUUID)
	$(TESTUUID)
	
#misc 
.PHONY: clean
.PHONY: cleanall
clean:
	rm $(BIN)/*.ali $(BIN)/*.o 2> /dev/null
cleanall: clean
	rm $(TESTUUID) $(LIB) 2> /dev/null

