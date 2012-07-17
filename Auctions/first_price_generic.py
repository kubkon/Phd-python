#!/usr/bin/env python2.7
# encoding: utf-8
"""
first_price_generic.py

Created by Jakub Konka on 2011-03-04.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os
import numpy as np
import scipy.integrate as integrate
import matplotlib.pyplot as plt


def main():
	iterations = 100
	N = 2
	avg_revenue = []
	revenue = []
	for i in range(iterations):
		bids = map(lambda x: x*(N-1)/N, (np.random.uniform(0,1) for n in range(N)))
		bids.sort()
		revenue.append(bids[N-1])
		avg_revenue.append(np.average(revenue))
	# Plot & save the figures
	# Figure 1: Pseudo-random draws vs. Average revenue
	plt.figure()
	r1 = plt.plot(range(iterations), avg_revenue, 'ro')
	theory = integrate.quad(lambda t,n=N: n*(n-1)*t*(1-t)*t**(n-2), 0, 1)
	r2 = plt.axhline(theory[0], ls='dotted')
	plt.title('First-price sealed-bid IPV with N={0} bidders\n(valuations drawn from uniform distribution)'.format(N))
	plt.xlabel('Number of iterations')
	plt.ylabel('Average revenue of the seller')
	plt.legend((r1, r2), ('Numerical results', 'Theoretical prediction'), 'upper right')
	plt.savefig('fpa_avg_revenue_'+ str(N) +'.pdf')
	plt.close('all')


if __name__ == '__main__':
	main()

