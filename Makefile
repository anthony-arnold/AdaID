VERSION=1.0
LIB=./lib/libadaid.so
DEBUG=./lib/libadaid.a
TEST=./bin/test
DIST=adaid_$(VERSION).orig.tgz
DIST_NAME=adaid-$(VERSION)
GNAT=gnat
RM=rm
CHMOD=chmod
MKDIR=mkdir -p
CP=cp


.PHONY: debug test install installclean clean cleanall all support remove dist

#library
$(LIB): src/*.adb src/*.ads include/*.ads adaid.gpr
	$(GNAT) make -Padaid.gpr

adaid.gpr: adaid.gpr.in
	@cat $< | sed 's/%VERSION%/$(VERSION)/' > $@

#installation
INSTALL = $(shell which $(GNAT) 2> /dev/null | sed -e 's/\/bin\/gnat.*//')
INCLUDE= $(INSTALL)/share/ada/adainclude/adaid
LIBRARY=$(INSTALL)/lib
ALI=$(INSTALL)/lib/ada/adalib/adaid
GPR   = $(INSTALL)/share/ada/adainclude

support/adaid.gpr: support/adaid.gpr.in
	@cat $< | sed  -e 's!%LIB_DIR%!$(LIBRARY)!' \
									-e 's!%SRC_DIR%!$(INCLUDE)!' \
									-e 's!%ALI_DIR%!$(ALI)!' > $@



installclean:
ifeq ($(INSTALL),)
	@echo 'Error when installing: $$INSTALL is empty...'
	@echo "Please set an installation path before installing"
else
	-$(CHMOD) -f -R 777 $(ALI)
	$(RM) -fr $(ALI)
	-$(CHMOD) -f -R 777 $(INCLUDE)
	$(RM) -fr $(INCLUDE)
	$(RM) -f $(GPR)/adaid.gpr
	$(RM) -f $(GPR)/adaid_debug.gpr
endif

install: installclean support/adaid.gpr
ifneq ($(INSTALL),)
	$(MKDIR) $(GPR)
	$(MKDIR) $(LIBRARY)
	$(MKDIR) $(INCLUDE)
	$(MKDIR) $(ALI)
	$(CP) -r ali/* $(ALI)
	$(CP) support/*.gpr $(GPR)
	$(CP) -r lib/* $(LIBRARY)
	$(CP) -r include/* $(INCLUDE)
	@echo '--  AdaID has been installed.'
endif

remove: installclean


#debug library
debug: support/adaid_debug.gpr $(DEBUG)

support/adaid_debug.gpr: support/adaid_debug.gpr.in
	@cat $< n | sed  -e 's!%LIB_DIR%!$(LIBRARY)!' \
									-e 's!%SRC_DIR%!$(INCLUDE)!' \
									-e 's!%ALI_DIR%!$(ALI)!' > $@

$(DEBUG): src/*.adb src/*.ads include/*.ads
	$(GNAT) make -Padaid_debug.gpr

#test executable
$(TEST): $(LIB) include/*.ads test/*.adb test/*.ads
	$(GNAT) make -Ptest.gpr

#run tests
test: $(TEST)
	$(TEST)

#make a tarball of the source
dist: $(DIST)

THIS=$(shell basename `pwd`)
$(DIST): cleanall
	cd ..;\
	tar caf $@ \
				--exclude-vcs --exclude=*~ --exclude=.gitignore --exclude=*.tgz\
				--xform 's!$(THIS)!$(DIST_NAME)!'\
				$(THIS)

#misc
all: $(LIB) $(TEST)

clean:
	$(RM) -f obj/*.* obj/test/*.* obj/debug/*.* ali/*.* ali/debug/*.* 2> /dev/null
	
cleanall: clean
	$(RM) -f support/*.gpr bin/* lib/* adaid.gpr  2> /dev/null

