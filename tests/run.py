# -*- coding: utf-8 -*-
"""
    sqlp unit tests
    ~~~~~~~~~~~~~~~

    Usage::

        python run.py [testfile ...]


    :copyright: Copyright 2014 by the sqlp team, see AUTHORS.
    :license: BSD, see COPYING for details.
"""

from __future__ import print_function

import sys, os

# only find tests in this directory
if os.path.dirname(__file__):
    os.chdir(os.path.dirname(__file__))


try:
    import nose
    use='nose'
except ImportError:
    try:
        import nose2
        use='nose2'
    except ImportError:
        print('nose is required to run the sqlp test suite')
        sys.exit(1)

try:
    # make sure the current source is first on sys.path
    sys.path.insert(0, '..')
    import sqlp
except SyntaxError as err:
    print('Syntax error: %s' % err)
    sys.exit(1)
except ImportError as err:
    print('Cannot find sqlp to test: %s' % err)
    sys.exit(1)
else:
    print('sqlp %s test suite running (Python %s)...' %
          (sqlp.__version__, sys.version.split()[0]))

if use == 'nose':
    nose.main()
else:
    nose2.discover()
