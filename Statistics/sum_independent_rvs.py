#!/usr/bin/env python
# encoding: utf-8
"""
sum_independent_rvs.py

Created by Jakub Konka on 2011-06-12.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import scikits.statsmodels as sm


def main():
	n = 100000
	a = 1
	b = 2
	x = np.random.uniform(0,1,n)
	y = np.random.uniform(0,1,n)
	g = a*x + b*y
	# Figures...
	# Histogram of g
	plt.figure()
	m, bins, patches = plt.hist(g, n/200, normed=1)
	# ECDF of g
	plt.figure()
	ecdf = sm.tools.ECDF(g)
	x = np.linspace(min(g), max(g))
	g_x = ecdf(x)
	plt.step(x, g_x)
	if b >= 1:
		x1 = np.linspace(0,1,1000)
		x2 = np.linspace(1,b,1000)
		x3 = np.linspace(b,b+1,1000)
		f1 = lambda x: 1/(2*b) * x**2
		f2 = lambda x: (1/b)*(x-1) + 1/(2*b)
		f3 = lambda x: (b+1)/b*x - 1/(2*b)*x**2 - b/2 - 1/(2*b)
	else:
		x1 = np.linspace(0,b,1000)
		x2 = np.linspace(b,1,1000)
		x3 = np.linspace(1,b+1,1000)
		f1 = lambda x: 1/(2*b) * x**2
		f2 = lambda x: x-b + b/2
		f3 = lambda x: (b+1)/b*x - 1/(2*b)*x**2 - b/2 - 1/(2*b)
	plt.plot(x1, f1(x1), 'r')
	plt.plot(x2, f2(x2), 'r')
	plt.plot(x3, f3(x3), 'r')
	plt.grid()
	plt.show()


if __name__ == '__main__':
	main()

