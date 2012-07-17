#!/usr/bin/env python2.7
# encoding: utf-8
"""
second_price_n_2.py

Created by Jakub Konka on 2011-02-28.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt


def do_randomise():
	'''Returns two (pseudo-)random variates of a uniform (pseudo-)random variable
	with support [0,1]'''
	x = np.random.uniform(0, 1)
	y = np.random.uniform(0, 1)
	return x, y


def main():
	iterations = (1,2,5,10,100,1000,10000,100000,1000000)
	avg_revenue = []
	avg_pay_1 = []
	avg_pay_2 = []
	for val in iterations:
		bid_1 = np.zeros(val) # Bids of the bidder 1 initialised to zeros
		bid_2 = np.zeros(val) # Bids of the bidder 2 initialised to zeros
		for i in range(val):
			(x, y) = do_randomise()
			bid_1[i] = x
			bid_2[i] = y
		revenue = [min(i, j) for i, j in zip(bid_1, bid_2)] # Revenues of the seller
		pay_1 = [j for i, j in zip(bid_1, bid_2) if i >= j] # Payments of the bidder 1
		pay_2 = [i for i, j in zip(bid_1, bid_2) if j >= i] # Payments of the bidder 2
		avg_revenue.append(np.average(revenue))
		avg_pay_1.append(np.sum(pay_1)/np.size(bid_1))
		avg_pay_2.append(np.sum(pay_2)/np.size(bid_2))
	# Plot & save the figures
	# Figure 1: Pseudo-random draws vs. Average revenue
	plt.figure()
	r1 = plt.semilogx(iterations, avg_revenue, 'ro')
	r2 = plt.axhline(0.333333, ls='dotted')
	plt.xlabel('Pseudo-random draws')
	plt.ylabel('Average revenue of the seller')
	plt.legend((r1, r2), ('Numerical results', 'Theoretical prediction'), 'upper right')
	#plt.savefig('revenue.pdf')
	plt.show()
	# Figure 2: Pseudo-random draws vs. Average payment by each bidder
	plt.figure()
	p1 = plt.semilogx(iterations, avg_pay_1, 'ro')
	p2 = plt.semilogx(iterations, avg_pay_2, 'b*')
	p3 = plt.axhline(0.166666, ls='dotted')
	plt.xlabel('Pseudo-random draws')
	plt.ylabel('Average monetary payment')
	plt.legend((p1, p2, p3), ('Bidder 1', 'Bidder 2', 'Theoretical prediction'), 'upper right')
	#plt.savefig('payment.pdf')
	plt.show()
	#plt.close('all')
	

if __name__ == '__main__':
	main()

