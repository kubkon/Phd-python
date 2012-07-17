#!/usr/bin/env python3
# encoding: utf-8
"""
combinations.py

Created by Jakub Konka on 2012-05-18.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os


def combinations(n):
  num = int((n-1) * n / 2)
  l = 0
  r = 1
  for i in range(num):
    print("{} {}".format(l, r))
    if (r == n - 1):
      l += 1
      r = l
    r += 1
    

if __name__ == '__main__':
  combinations(5)

