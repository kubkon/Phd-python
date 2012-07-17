#!/usr/bin/env python2.7
# encoding: utf-8
"""
function_random_variable.py

Created by Jakub Konka on 2011-04-25.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import scikits.statsmodels as sm
import matplotlib.pyplot as plt
from matplotlib import rc


rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def plot_pdf(n):
	w = 0.5
	r = 0.9
	X = np.random.uniform(0,1,n)
	Y = map(lambda x: w*x + (1-w)*r, X)
	# CDF estimate of X
	plt.figure()
	ecdf_x = sm.tools.ECDF(X)
	x = np.linspace(min(X), max(X))
	f_x = ecdf_x(x)
	plt.step(x, f_x)
	plt.xlabel(r"$x$")
	plt.ylabel(r"Empirical CDF of $X \sim U(0,1)$")
	plt.grid()
	# PDF estimate of Y
	plt.figure()
	count, bins, patches = plt.hist(Y, n/200, normed=1)
	# CDF estimate of Y
	plt.figure()
	ecdf_y = sm.tools.ECDF(Y)
	y = np.linspace(min(Y), max(Y))
	f_y = ecdf_y(y)
	plt.step(y, f_y)
	plt.xlabel(r"$y$")
	plt.ylabel(r"Empirical CDF of $Y=w\cdot X + (1-w)\cdot r$")
	plt.grid()


if __name__ == '__main__':
	N = 100000 # number of random draws
	plot_pdf(N)
	plt.show()

