#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
####################################################################### 
#Compiles the code into a ebin dir. relative to the source dir. 

EUNIT   := $(HOME)/workplace/eunit
ERLC 	:= erlc
GEN 	:= beam

EFLAGS := -pa ebin +debug_info

INCLUDE := -Iinclude -I$(EUNIT)/include
EBIN 	:= ebin
SRC     := $(wildcard src/*.erl)
HEADERS := $(wildcard $(INCLUDE)/*.hrl)	
CODE  	:= $(patsubst src/%.erl, ebin/%.beam, $(SRC))
DOTSRC  := $(wildcard src/*.app.src)
DOTAPP  := $(patsubst src/%.app.src, ebin/%.app, $(DOTSRC))


.PHONY: clean all

$(EBIN)/%.beam: src/%.erl
	$(ERLC) $(INCLUDE) -pa $(EUNIT)/ebin -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

all: $(CODE) 

clean:
	rm -f $(EBIN)/*.beam
