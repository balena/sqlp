# -*- coding: utf-8 -*-
"""
    Test suite for SQL files as input
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    sqlp should be able to recognize all input files as valid

    :copyright: Copyright 2014 by the sqlp team, see AUTHORS.
    :license: BSD, see COPYING for details.
"""

import os
import glob
import unittest
from sqlp import *
from tests.utils import load_file

class ReadFilesTest(unittest.TestCase):

  @classmethod
  def setUpClass(cls):
    cls.parser = SQLParser()

  def test_files(self):
    for filename in glob.glob(os.path.join('v6_pc_isql','*.sql')):
      yield self.check_file, filename
      
  def check_file(self, filename):
    self.parser.parse(open(filename).read(), filename)
