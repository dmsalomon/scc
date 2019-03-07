import sys
sys.path.insert(0, '..')

import io
import scan
import unittest

class TestScanner(unittest.TestCase):
    def test_ID(self):
        txt = """
            foo
            FOO
            FOR
            RETURN
            _foo
            __for__
            _
        """
        m = scan.Plexer(io.StringIO(txt))
        for t in m:
            self.assertIsInstance(t, scan.PlexToken)
            self.assertEqual(t.type, 'ID')

    def test_KW(self):
        txt = """
            for foreach in
            if else elsif
            while do end
            print return
            array tuple
            local global defun
            """
        m = scan.Plexer(io.StringIO(txt))
        for t in m:
            self.assertEqual(t.value, t.text)
            self.assertIn(t.value, scan.Plexer.reserved)
            self.assertEqual(t.type, scan.Plexer.reserved.get(t.value))

    def test_len(self):
        txt = "this+that-<->\nwhy"
        lx = [4, 1, 4, 1, 3, 3]
        m = scan.Plexer(io.StringIO(txt))
        for l, t in zip(lx, m):
            self.assertEqual(len(t.text), l)

    def test_lineno(self):
        txt = "\n\r\n\r\r\r\n\n"
        m = scan.Plexer(io.StringIO(txt))
        for t in m: pass
        self.assertEqual(m.lineno, 7)

    def test_ID_trun(self):
        t = 'abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz'
        txt = f'p\n\n<->+++{t}\n\n'
        m = scan.Plexer(io.StringIO(txt))
        for _ in range(5): m.next()
        c = m.next()
        self.assertEqual(c.type, 'ID')
        self.assertEqual(c.text, t)
        self.assertNotEqual(c.value, t)
        self.assertEqual(c.value, t[:31])
        self.assertEqual(c.begpos, 7)
        self.assertEqual(c.endpos, 7+len(t)-1)
        self.assertEqual(c.lineno, 3)
