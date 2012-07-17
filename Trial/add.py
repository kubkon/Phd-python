#!/usr/bin/env python3
# encoding: utf-8
"""
add.py

Created by Jakub Konka on 2010-11-16.
Copyright (c) 2010 University of Strathclyde. All rights reserved.
"""

import sys
import os


def add(number1, number2):
	'''Adds two number together
	
	Keyword arguments:
	number1 -- the first number
	number2 -- the second number
	
	Returns: int or float
	'''
	result = number1 + number2
	return '{0:.11f}'.format(result)


if __name__ == '__main__':
	print(add(1,9))

