#!/usr/bin/env python2.7
# encoding: utf-8
"""
first_price_procurement.py

Created by Jakub Konka on 2011-03-09.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integrate
import matplotlib.pyplot as plt


def main():
	iterations = 1000
	N = 2
	avg_price = []
	price = []
	for i in range(iterations):
		costs = [np.random.uniform(1,2) for n in range(N)]
		bids_1 = ((N-1) / (2-c)**(N-1) * integrate.quad(lambda x,n=N: (2-x)**(N-2) / x, c, 2)[0] for c in costs)
		bids = map(lambda x: 1/x, bids_1)
		bids.sort()
		price.append(bids[0])
		avg_price.append(np.average(price))
	# Plot & save the figures
	# Figure 1: Pseudo-random draws vs. Average price
	plt.figure()
	r1 = plt.plot(range(iterations), avg_price, 'ro')
	theory = integrate.quad(lambda t, n=N: n*(2-t)**(n-1) / \
	 						((N-1) / (2-t)**(N-1) * \
							integrate.quad(lambda x,n=N: (2-x)**(N-2) / x, t, 2)[0]), 1, 2)
	r2 = plt.axhline(theory[0], ls='dotted')
	plt.title('First-price sealed-bid procurement auction with N={0} bidders\n(valuations drawn from uniform distribution)'.format(N))
	plt.xlabel('Number of iterations')
	plt.ylabel('Average price for the buyer')
	plt.legend((r1, r2), ('Numerical results', 'Theoretical prediction'), 'upper right')
	plt.savefig('fpa_avg_price_'+ str(N) +'.pdf')
	plt.close('all')
	#plt.show()


if __name__ == '__main__':
	main()

