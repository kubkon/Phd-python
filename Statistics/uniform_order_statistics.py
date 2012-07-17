#!/usr/bin/env python2.7
# encoding: utf-8
"""
uniform_order_statistics.py

Created by Jakub Konka on 2011-04-18.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import scipy as sp
import matplotlib.pyplot as plt


def plot_pdf(order, N, iterations):
	order_stats = []
	for it in range(iterations):
		numbers = [np.random.uniform(0,1) for i in range(N)]
		numbers.sort()
		order_stats.append(numbers[order-1])
	plt.figure()
	n, bins, patches = plt.hist(order_stats, iterations/20, normed=1, facecolor='green')
	y = lambda x: int(sp.factorial(N))/(int(sp.factorial(N-order))*int(sp.factorial(order-1))) * x**(order-1) * (1-x)**(N-order)
	plt.plot(bins, y(bins), 'r--', linewidth=3)


if __name__ == '__main__':
	iterate = 10000
	N = 5
	for i in range(N):
		plot_pdf(i+1,N,iterate)
	plt.show()

