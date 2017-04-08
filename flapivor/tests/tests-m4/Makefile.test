# -*- Makefile -*-
.PHONY: check bad good clean all clean-silent annnounce expected
all:

-include ../../../../Makefile.local

PROGRAM=../../../../$(TARGET) $(OPTIONS)
-include ../../extension

OPTIONS=$(shell if test -f options; then cat options; else echo ""; fi)
BAD_INPUTS=$(shell if pwd | grep bad 1> /dev/null; \
	     then find . -name "*.$(EXT)" | sort; fi)
BAD_OUTPUTS=$(BAD_INPUTS:.$(EXT)=.bad-output)
GOOD_INPUTS=$(shell if pwd | grep good 1> /dev/null; \
	      then find . -name "*.$(EXT)" | sort; fi)
GOOD_OUTPUTS=$(GOOD_INPUTS:.$(EXT)=.good-output)
GOOD_EXPECTED=$(GOOD_INPUTS:.$(EXT)=.expected)

clean-expected:
	@ rm --force *.expected

expected: clean-expected $(GOOD_EXPECTED)

%.expected: %.$(EXT)
	@ ($(PROGRAM) $< > $@) || (echo "$@ has failed."); touch $@

%.bad-output:%.$(EXT)
	@ echo TEST >> bad-counter;					 \
	($(PROGRAM) $< 2> $@ 1> $@ && echo "[ KO ]   --> " $<		 \
	" should have failed !")					 \
	 || (echo "[XFAIL] " $<; echo 'XF' >> bad-ok-counter);
	@ if [ x$$VERBOSE = x1 ]; then					 \
	echo;								 \
	echo "INPUT: ";							 \
	cat $<;								 \
	echo;								 \
	echo "OUTPUT: ";						 \
	cat $@;								 \
	echo "--------------------------------------------------------"; \
	fi

%.good-output:%.$(EXT)
	@ (echo TEST >> good-counter);					 \
	  (($(PROGRAM) $< 1> $@ 2> $@)					 \
	   && (diff -uwB $@ $*.expected 1> /dev/null 2> /dev/null)	 \
	  && (echo "[ OK ]  " $<; (echo "OK $@" >> good-ok-counter)))	 \
	  || (echo "[ KO ]   --> " $<); if [ x$$VERBOSE = x1 ]; then	 \
	echo;								 \
	echo "INPUT:";							 \
	cat $<;								 \
	echo;								 \
	echo "OUTPUT: ";						 \
	cat $@;								 \
	echo "EXPECTED: ";						 \
	cat $*.expected;						 \
	echo "--------------------------------------------------------"; \
	fi

clean-bad-counters:
	@ rm --force bad-ok-counter bad-counter

clean-good-counters:
	@ rm --force good-counter good-ok-counter

bad: clean-bad-counters $(BAD_OUTPUTS)
	@ echo $(BAD_INPUTS)
	@ touch bad-ok-counter
	@ touch bad-counter
	@ echo "BAD:  "`wc -l bad-ok-counter | cut -f1 -d' '` \
	/ `wc -l bad-counter | cut -f1 -d' '`

good: clean-good-counters $(GOOD_OUTPUTS)
	@ touch good-ok-counter
	@ touch good-counter
	@ echo "GOOD: "`wc -l good-ok-counter | cut -f1 -d' '` \
	/ `wc -l good-counter | cut -f1 -d' '`

announce:
	@ echo "========================================================";
	@ echo "In "`pwd | sed 's/.*\/\(.*\/.*\)/\1/' `

check: announce clean-silent good bad

clean:
	$(RM) --force *.good-output *.bad-output *counter *~

clean-silent:
	@ $(RM) --force *.good-output *.bad-output *counter *~
