#!/usr/bin/env python
# encoding: utf-8
"""
expected_prices.py

Created by Jakub Konka on 2012-01-20.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rc

rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def plot(n, prices, rep, w):
	N = [i+1 for i in range(n)]
	fig = plt.figure()
	ax = fig.add_subplot(111)
	ax.plot(N, prices, 'b*')
	plt.xlabel(r"Iterations, $n$")
	plt.ylabel(r"Price, $b'$")
	plt.ylim([0,1.5])
	plt.text(0.65, 1.12, \
			 "Reps: {0}\nw: {1}\nAverage price: {2}".format(rep, w, np.mean(prices)), \
			 fontsize=13, \
			 horizontalalignment='left', \
			 verticalalignment='top', \
			 transform=ax.transAxes)
	plt.grid()
	plt.savefig("expected_prices.pdf")


def calc_price(rep, w):
	costs = [np.random.uniform(0,1) for i in range(2)]
	# Calculate equilibrium bids
	b1 = 0.5 + 0.5*costs[0] - (1-w)*(rep[0]-rep[1])/(3*w)
	b2 = 0.5 + 0.5*costs[1] - (1-w)*(rep[1]-rep[0])/(3*w)
	# Find the winner and return appropriate price
	beta1 = w*b1 + (1-w)*rep[0]
	beta2 = w*b2 + (1-w)*rep[1]
	price = 0
	if beta1 < beta2: price = b1
	elif beta1 > beta2: price = b2
	else: price = 0.5*(b1 + b2)
	return price


def run():
	n = int(sys.argv[1])
	rep = [0.0, 1.0]
	w = 0.75
	prices = [calc_price(rep, w) for i in range(n)]
	plot(n, prices, rep, w)


if __name__ == '__main__':
	run()

