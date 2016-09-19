.PHONY: check bad good clean all clean-silent annnounce expected

all:

announce:
	@ if [ x$$VERBOSE != x1 ]; then \
	    echo "HINT: set VERBOSE=1, to show tests inputs and results."; \
          fi

install-test:
	@ for i in */*/bad */*/good; do		\
	  echo "Install in $$i"; 		\
	  cp Makefile.test $$i/Makefile;	\
	done

build:
	$(MAKE) -C ..

check:	build announce
	@ for i in */*/bad */*/good; do				    \
	  $(MAKE) --silent -C $$i check 2> /dev/null || true;    	    \
	done;
	@ echo "--------------------------------------------------------";
	@ echo "Global results:"
	@ (cat */*/bad/bad-ok-counter 2> /dev/null) > bad-ok-counter || true;
	@ (cat */*/bad/bad-counter 2> /dev/null) > bad-counter || true;
	@ echo "BAD:   "`wc -l bad-ok-counter | cut -f1 -d' '`		    \
	/ `wc -l bad-counter | cut -f1 -d' '`
	@ (cat */*/good/good-ok-counter 2> /dev/null) > good-ok-counter \
          || true;
	@ (cat */*/good/good-counter 2> /dev/null) > good-counter || true;
	@ echo "GOOD:  "`wc -l good-ok-counter | cut -f1 -d' '`		    \
	/ `wc -l good-counter | cut -f1 -d' '`
	@ echo "--------------------------------------------------------";

clean:
	@ for i in */*/bad */*/good; do					    \
	  $(MAKE) --silent -C $$i clean 2> /dev/null || true;    	    \
	done;
	@ rm --force *counter *~

expected: build
	@ for i in */*/bad */*/good; do					    \
	  $(MAKE) --silent -C $$i expected 2> /dev/null || true;    	    \
	done;
