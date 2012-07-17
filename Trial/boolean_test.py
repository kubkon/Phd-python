#!/usr/bin/env python3
# encoding: utf-8
"""
booleanTest.py

Created by Jakub Konka on 2010-11-21.
Copyright (c) 2010 Uni of Strathclyde. All rights reserved.
"""

import sys
import os


def boolean_test(anything):
	'''Tests whether the statement is True or False
	
	Keyword arguments:
	anything -- the statement to be evaluated
	
	Returns: Boolean
	'''
	if anything:
		print("The statement is True.\n")
	else:
		print("The statement is False.\n")


if __name__ == '__main__':
	boolean_test([1])

