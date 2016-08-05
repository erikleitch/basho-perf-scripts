ERLSRCOBJ =  $(patsubst %.erl,%.beam,$(wildcard src/*.erl))
ERLALLOBJ =  $(patsubst src/%,%,$(ERLSRCOBJ))

all: dirs erlmods ccmods

erlmods: ${ERLALLOBJ}

%.beam: src/%.erl
	erlc -o ebin $<

CCSRCOBJ =  $(patsubst %.cc,%.o,$(wildcard c_src/*.cc))
CCALLOBJ =  $(patsubst c_src/%,%,$(CCSRCOBJ))

ccmods: ${CCALLOBJ}

%.o: c_src/%.cc
	g++ -o priv/$*.so -fpic -shared c_src/$*.cc -I$(_KERL_ACTIVE_DIR)/usr/include -undefined dynamic_lookup -ls\
tdc++

dirs:
	@mkdir -p ebin
	@mkdir -p priv

clean:
	\rm -rf priv
	\rm -rf ebin
