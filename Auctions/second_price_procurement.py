#!/usr/bin/env python2.7
# encoding: utf-8
"""
second_price_procurement.py

Created by Jakub Konka on 2011-03-08.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import scipy.integrate as integrate


def main():
	iterations = 1000
	N = 3
	avg_price = []
	price = []
	for val in range(iterations):
		bids = [np.random.uniform(1,2) for n in range(N)]
		bids.sort()
		price.append(bids[1])
		avg_price.append(np.average(price))
	# Plot & save the figures
	# Figure 1: Pseudo-random draws vs. Average price
	plt.figure()
	r1 = plt.plot(range(iterations), avg_price, 'ro')
	theory = integrate.quad(lambda t,n=N: t*n*(n-1)*(t-1)*(2-t)**(n-2), 1, 2)
	r2 = plt.axhline(theory[0], ls='dotted')
	plt.title('Second-price sealed-bid procurement auction with N={0} bidders\n(valuations drawn from uniform distribution)'.format(N))
	plt.xlabel('Number of iterations')
	plt.ylabel('Average price for the buyer')
	plt.legend((r1, r2), ('Numerical results', 'Theoretical prediction'), 'upper right')
	#plt.savefig('spa_avg_price_'+ str(N) +'.pdf')
	#plt.close('all')
	plt.show()


if __name__ == '__main__':
	main()
