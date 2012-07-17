#!/usr/bin/env python
# encoding: utf-8
"""
random_permutation.py

Created by Jakub Konka on 2011-05-15.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import math
from itertools import permutations


def generate(n):
	# 1. Generate ordered list of n integers
	numbers = [i+1 for i in range(n)]
	# 2. Pick a number from 1..n at random and swap
	# the number in that position in the list with the number at n
	# 3. Repeat for remaining n-1...1 numbers in the list
	segments = [i/n for i in range(n)]
	for i in reversed(range(n)):
		u = np.random.uniform(0,1)
		index = -1
		for seg in segments:
			if u >= seg: index += 1
		# index = int(np.rint(i*u))
		numbers[index], numbers[i] = numbers[i], numbers[index]
	return tuple(numbers)


if __name__ == '__main__':
	n = 3
	num = 100000
	# Verification
	# Generate num of random permutations of n numbers
	result = [generate(n) for i in range(num)]
	# Gather statistics (here, prob of occurrence of each permutation)
	count = result.count
	freq = [count(item) for item in set(result)]
	prob = map(lambda x: x/num, freq)
	print(prob)

