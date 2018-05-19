#!/usr/bin/env python3

import os
import unittest


class MyTests(unittest.TestCase):
    def _my_test_output(self, cmd, want_output):
        got_output = os.popen(cmd).read()
        self.assertEqual(
            got_output, want_output,
            'The command "%s" yields good output' % (cmd))

    def test_output(self):
        self._my_test_output(
            '< ./tests/data/24.fcs.board perl ./contrib/input-from-fc-solve',
            open('./tests/data/24.hs.board', 'r').read())
        self._my_test_output(
            './solver.exe ./tests/data/24.hs.board',
            open('./tests/data/24.hs.output.txt', 'r').read())


if __name__ == '__main__':
    from pycotap import TAPTestRunner
    suite = unittest.TestLoader().loadTestsFromTestCase(MyTests)
    TAPTestRunner().run(suite)
