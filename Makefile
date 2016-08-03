.PHONY: check

all:

check:
	gosh -Ilib test/frame-test.scm
	gosh -Ilib test/server-request-test.scm
