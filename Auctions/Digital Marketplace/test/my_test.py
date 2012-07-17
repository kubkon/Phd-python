#!/usr/bin/env python
# encoding: utf-8
"""
my_test.py

Created by Jakub Konka on 2011-05-17.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import operator
from matplotlib import rc


rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def test(w, case):
	# Parameters
	# Types:
	pmin = 0
	pmax = 1
	c1 = np.ones(1000)*np.random.uniform(pmin, pmax)
	c2 = np.ones(1000)*np.random.uniform(pmin, pmax)
	# Reputations:
	ops = {"<": operator.lt, ">": operator.gt, "=": operator.eq}
	r1 = np.random.uniform(0, 1)
	r2 = np.random.uniform(0, 1)
	if ops[case] == operator.eq: r2 = r1
	while not ops[case](r1, r2):
		r2 = np.random.uniform(0, 1)
	# Plot bids versus submitted prices
	p = np.linspace(0, 3, 1000)
	f1 = lambda x: w*x + (1-w)*r1
	f2 = lambda x: w*x + (1-w)*r2
	plt.figure()
	plt.plot(p, f1(p), 'b')
	plt.plot(p, f2(p), 'r')
	plt.plot(p, f1(c1), 'b--')
	plt.plot(p, f2(c2), 'r--')
	plt.legend((r"$f_1(p)$", r"$f_2(p)$"), loc="upper left")
	plt.grid()
	plt.xlabel(r"Price, $p$")
	plt.ylabel(r"Bids, $f_i(p)$")
	plt.savefig('test.pdf')
	

if __name__ == '__main__':
	test(0.1, "<")

