#!/usr/bin/env python3
# encoding: utf-8
"""
assignment.py

Created by Jakub Konka on 2011-12-05.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys

def factorial(n):
	if n == 0 or n == 1:
		return 1
	return n * factorial(n-1)

def exponential(x):
	if x == 0:
		return 1
	est = 0
	for i in range(100):
		est += x**i / factorial(i)
	return est


if __name__ == '__main__':
	i = 40000
	# while True:
	# 	try:
	# 		exponential(i)
	# 		i += 1
	# 	except OverflowError:
	# 		break
	# print("Overflow error occurs for i = " + str(i))
	print(exponential(100))
