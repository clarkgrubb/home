#include <cppunit/ui/text/TestRunner.h>

#include "test_neighbors.h"

int main( int argc, char **argv)
{
  CppUnit::TextUi::TestRunner runner;
  runner.addTest(TestNeighbors::suite());
  runner.run();
  return 0;
}
