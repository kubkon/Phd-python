#!/usr/bin/env python2.7
# encoding: utf-8
"""
contraction_mapping.py

Created by Jakub Konka on 2011-07-13.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt


def iterate(n, seed):
	# Solve for Ax=x using fixed point theorem
	conv = []
	conv.append(seed)
	for i in range(n):
		conv.append(np.cos(conv[i]))
	# Plot the results
	x = np.linspace(0,1,1000)
	# Figure 1
	plt.figure()
	plt.plot(x, x, 'b')
	plt.plot(x, np.cos(x), 'r')
	plt.plot(conv[0:n-2], conv[1:n-1], 'd')
	plt.grid()
	plt.show()
	

if __name__ == '__main__':
	n = 100
	seed = 0
	iterate(n, seed)

