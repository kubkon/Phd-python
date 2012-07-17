#!/usr/bin/env python2.7
# encoding: utf-8
"""
multivariate_normal.py

Created by Jakub Konka on 2011-04-19.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt


def main():
	mu = [0,0]
	cov = [[1,0],[0,1]]
	x,y = np.random.multivariate_normal(mu, cov, 10000).T
	plt.figure()
	plt.plot(x, y, 'x')
	plt.figure()
	plt.hexbin(x,y)
	plt.show()

if __name__ == '__main__':
	main()

