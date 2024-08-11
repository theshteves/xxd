#!/usr/bin/env python3
import subprocess
import sys
import unittest


class TestScript(unittest.TestCase):
    def test_python(self):
        with open('zen.txt', 'r') as input_file:
            result = subprocess.run([sys.executable, 'xxd.py'], 
                                    stdin=input_file, 
                                    capture_output=True, 
                                    text=True)
        
        self.assertEqual(result.returncode, 0, f'Script failed with error:\n{result.stderr}')
        self.assertIn('  f those!', result.stdout)


if __name__ == '__main__':
    unittest.main()
