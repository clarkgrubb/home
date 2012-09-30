all:
	find . -name '*.rb' | xargs ruby -c

TAGS:
	find . -name '*.rb' | xargs etags

check:
	echo impl

clean:
	find . -name 'test*.rb' | xargs ruby

html:
	echo implement me

install:
	echo implement me

#
#
test: check

tags: TAGS
