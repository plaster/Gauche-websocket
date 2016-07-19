.PHONY: check

all:

check:
	gosh -Ilib test/parser-test.scm
