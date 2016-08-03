.PHONY: check

all:

check:
	gosh -Ilib test/frame-test.scm
