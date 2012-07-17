#!/usr/bin/env python2.7
# encoding: utf-8
"""
strong_law_large_numbers.py

Created by Jakub Konka on 2011-04-20.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

from __future__ import division
import sys
import os
import numpy as np
import matplotlib.pyplot as plt


def main():
	N = 10 # number of independent normally distributed random variables
	mu = 0 # mu (mean) of each r.v.
	sigma = 1 # sigma (variance) of each r.v.
	samples = 100000 # number of samples to be drawn
	outcomes = [np.random.normal(mu, sigma, samples) for i in range(N)]
	sum_of_outcomes = np.zeros(shape=(1,samples))
	for i in range(N):
		sum_of_outcomes += outcomes[i]
	# Figure 1 -- hist of one r.v.
	plt.figure()
	count, bins, patches = plt.hist(outcomes[0], 50, normed=1, facecolor='green')
	# Figure 2 -- hist of the sum of all r.v.
	plt.figure()
	count, bins, patches = plt.hist(sum_of_outcomes[0], 50, normed=1, facecolor='green')
	plt.show()

if __name__ == '__main__':
	main()

