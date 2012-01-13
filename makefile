SRC=./src

FILES=$(SRC)/adaid.ads $(SRC)/adaid.adb
OBJ=adaid.o

testuuid: $(FILES) $(OBJ)
	gnatmake testuuid -I$(SRC)

test: testuuid
	./testuuid

adaid.o: $(FILES)
	gnatmake adaid -I$(SRC)

