# Copyright (C) 2014 Guilherme Balena Versiani, guibv42@gmail.com
#
# This setup script is part of python-sqlparse and is released under
# the BSD License: http://www.opensource.org/licenses/bsd-license.php.

import re
import sys

try:
    from setuptools import setup, find_packages
    packages = find_packages(exclude=('tests',))
except ImportError:
    if sys.version_info[0] == 3:
        raise RuntimeError('distribute is required to install this package.')
    from distutils.core import setup
    packages = ['sqlp']


def get_version():
    """parse __init__.py for version number instead of importing the file

    see http://stackoverflow.com/questions/458550/standard-way-to-embed-version-into-python-package
    """
    VERSIONFILE='sqlp/__init__.py'
    verstrline = open(VERSIONFILE, "rt").read()
    VSRE = r'^__version__ = [\'"]([^\'"]*)[\'"]'
    mo = re.search(VSRE, verstrline, re.M)
    if mo:
        return mo.group(1)
    else:
        raise RuntimeError('Unable to find version string in %s.'
                           % (VERSIONFILE,))


LONG_DESCRIPTION = """
``sqlp`` is a full SQL parser module, intended to provide a SQL AST and
perform some simple DDL optimizations.

It is built over PLY.

Visit the `project page <http://code.google.com/p/sqlp>`_ for
additional information and documentation.
"""

VERSION = get_version()


kwargs = {}
if sys.version_info[0] == 3:
    kwargs['use_2to3'] = True


setup(
    name='sqlp',
    version=VERSION,
    packages=packages,
    description='SQL parser and AST generator',
    author='Guilherme Balena Versiani',
    author_email='guibv42@gmail.com',
    long_description=LONG_DESCRIPTION,
    license='BSD',
    url='http://code.google.com/p/sqlp',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.4',
        'Programming Language :: Python :: 2.5',
        'Programming Language :: Python :: 2.6',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.2',
        'Programming Language :: Python :: 3.3',
        'Topic :: Database',
        'Topic :: Software Development'
    ],
    **kwargs
)
