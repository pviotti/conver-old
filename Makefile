REBAR = $(shell pwd)/rebar3
SHELL = /bin/bash

all: compile	

compile:
	$(REBAR) compile
	
clean:
	$(REBAR) clean

dialyzer:
	$(REBAR) dialyzer
