CXX := g++

CXXFLAGS := -std=c++0x -O2


SOURCE_FILES := foo.cpp

TEST_SOURCE_FILES := test_foo.cpp

TARGETS := test_runner foo


OBJECT_FILES := $(patsubst %.cpp,%.o,$(SOURCE_FILES))

HEADER_FILES := $(patsubst %.cpp,%.h,$(SOURCE_FILES))

TEST_OBJECT_FILES := $(patsubst %.cpp,%.o,$(TEST_SOURCE_FILES))

TEST_HEADER_FILES := $(patsubst %.cpp,%.h,$(TEST_SOURCE_FILES))


all: $(TARGETS)

test_runner: test_runner.o $(TEST_OBJECT_FILES) $(OBJECT_FILES)
	$(CXX) $(CXXFLAGS) -o $@ $< $(OBJECT_FILES) \
	$(TEST_OBJECT_FILES) -lcppunit

foo: $(OBJECT_FILES)
	$(CXX) $(CXXFLAGS) -o $@ $< $(OBJECT_FILES)

# If any headers are modified, everything is recompiled.
# We don't try to determine the actual dependencies.
#
%.o: %.cpp $(HEADER_FILES) $(TEST_HEADER_FILES)
	$(CXX) $(CXXFLAGS) -c $<

test: test_runner
	./test_runner

clean:
	-rm *.o

clobber: clean
	-rm $(TARGETS)

apt.sudo:
	sudo apt-get install g++-4.6 libcppunit-dev valgrind

setup.sudo: apt.sudo

valgrind: test_runner
	valgrind ./test_runner
