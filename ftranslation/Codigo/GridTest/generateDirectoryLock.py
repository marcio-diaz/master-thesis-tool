#!/usr/bin/env python
# -*- coding: utf-8 -*-


import tempfile
import os.path





open ('.modaltesting.dir.lock', 'w').write (tempfile.mkdtemp (prefix = 'modaltesting-', dir = os.path.expanduser (os.path.expandvars ('~'))))
