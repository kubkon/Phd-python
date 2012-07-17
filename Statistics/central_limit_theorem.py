#!/usr/bin/env python2.7
# encoding: utf-8
"""
central_limit_theorem.py

Created by Jakub Konka on 2011-04-20.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import math as mh


def main():
	N = 100 # number of independent exponentially distributed random variables
	mu = 0.1 # mu (mean) of each r.v.
	sigma = mu**2 # sigma (variance) of each r.v.
	samples = 1000 # number of samples to be drawn
	outcomes = [np.random.exponential(mu, samples) for i in range(N)]
	sum_of_outcomes = np.zeros(shape=(1,samples))
	for i in range(N):
		sum_of_outcomes += outcomes[i]
	sum_of_outcomes = map(lambda x: (x - N*mu)/mh.sqrt(sigma*N), sum_of_outcomes[0])
	# Figure 1 -- hist of one r.v.
	plt.figure()
	count, bins, patches = plt.hist(outcomes[0], 50, normed=1, facecolor='green')
	# Figure 2 -- hist of the sum of all r.v.
	plt.figure()
	count, bins, patches = plt.hist(sum_of_outcomes, 50, normed=1, facecolor='green')
	plt.show()


if __name__ == '__main__':
	main()

