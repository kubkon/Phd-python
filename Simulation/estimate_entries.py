#!/usr/bin/env python
# encoding: utf-8
"""
estimate_entries.py

Created by Jakub Konka on 2011-05-15.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import math


def estimate(the_list, n, k):
	# Counting elements... (normally, we would count only those corresponding to X)
	count = the_list.count
	freq = {item: count(item) for item in set(the_list)}
	# Generate k r.v.s of the form X = n*U(0,1) + 1
	rv_sum = 0
	for i in range(k):
		X = int(np.rint(np.random.uniform(0,1)*(n-1))) + 1
		while X not in freq:
			X = int(np.rint(np.random.uniform(0,1)*(n-1))) + 1
		# if X not in freq: continue
		rv_sum += 1/freq[X]
	return n*rv_sum/k


if __name__ == '__main__':
	n = 1000
	k = 10
	iterations = 1000
	# Populate the list with random integers from 1..n
	the_list = [int(np.rint(np.random.uniform(0,n-1))) + 1 for i in range(n)]
	det_d = len(set(the_list))
	print("Actual value of d: {0}".format(det_d))
	# Estimate the number of distinct elements in the list
	est_d = [estimate(the_list, n, k) for i in range(iterations)]
	print("Estimated value of d: {0}".format(np.mean(est_d)))

