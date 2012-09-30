import unittest

class TestFoo(unittest.TestCase):
  def test_01(self):
    self.assertTrue(True, 'not True!')

if __name__ == '__main__':
  unittest.main()
