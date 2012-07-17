#!/usr/bin/env python
# encoding: utf-8
"""
my_test2.py

Created by Jakub Konka on 2011-06-06.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integral


def test(N, w):
	# Parameters
	pmin = 0
	pmax = 1
	# Types:
	types = [np.random.uniform(pmin, pmax) for i in range(N)]
	# Reputations:
	rep = [np.random.uniform(pmin, pmax) for i in range(N)]
	# Bids:
	inv_prices = ((N-1)/pmax / (1-c/pmax)**(N-1) * integral.quad(lambda x,n=N: (1-x/pmax)**(N-2) / x, c, pmax)[0] for c in types)
	prices = map(lambda x: 1/x, inv_prices)
	my_prices = [max([prices[i], 0.5 + (1-w)/w * (0.5-rep[i])]) for i in range(N)]
	# Compound bids:
	my_bids = map(lambda x,y: w*x + (1-w)*y, my_prices, rep)
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep)
	# Utilities:
	my_utility = []
	if my_bids[0] < my_bids[1]:
		my_utility.append(my_prices[0]-types[0])
		my_utility.append(0)
	else:
		my_utility.append(0)
		my_utility.append(my_prices[1]-types[1])
	utility = []
	if bids[0] < bids[1]:
		utility.append(prices[0]-types[0])
		utility.append(0)
	else:
		utility.append(0)
		utility.append(prices[1]-types[1])
	# Results:
	print("Types and reputations")
	print("Bidder 1: t1={0}, r1={1}".format(types[0], rep[0]))
	print("Bidder 2: t2={0}, r2={1}".format(types[1], rep[1]))
	print("\nMy bidding strategy")
	print("Bidder 1: p1={0}, b1={1}, u1={2}".format(my_prices[0], my_bids[0], my_utility[0]))
	print("Bidder 2: p2={0}, b2={1}, u2={2}".format(my_prices[1], my_bids[1], my_utility[1]))
	print("\nStandard FPA bidding strategy")
	print("Bidder 1: p1={0}, b1={1}, u1={2}".format(prices[0], bids[0], utility[0]))
	print("Bidder 2: p2={0}, b2={1}, u2={2}".format(prices[1], bids[1], utility[1]))
	

if __name__ == '__main__':
	test(2, 0.1)

