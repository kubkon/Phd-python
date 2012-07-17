#!/usr/bin/env python
# encoding: utf-8
"""
test_orig_dmarket.py

Created by Jakub Konka on 2011-05-09.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integral
import scipy.stats.mstats as mstats
import scikits.statsmodels as sm
import matplotlib.pyplot as plt
from operator import itemgetter
from matplotlib import rc


rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)


def simulate(N, pmin, pmax):
	'''This function simulates one stage of the auctioning game among
	the network operators.
	Keyword args:
	N -- number of bidders
	pmin -- minimum price/cost of the service
	pmax -- maximum price/cost of the service
	Returns: intersection (if exists) of the winning bidders for price weight->1
	'''
	# Service agent weights
	w_range = 1000
	w = np.linspace(0, 1, w_range)
	# Calculate prices
	max_n = 100 # maximum number of contracts that can be supported by the bidder
	beta = 1 # parameter characterising price evolution
	r = 75 # number of contract tenders already underway minus the current tender
	costs = [np.random.uniform(pmin, pmax) for i in range(N)] # costs drawn from P~U(pmin,pmax)
	rep = [np.random.uniform(0, 1) for i in range(N)] # reputation drawn from X~U(0,1)
	prices = map(lambda x: x + (np.exp(((r-1)/(max_n-1))**beta) - 1) / (np.exp(beta) - 1) * (pmax-x), costs) # pricing vector
	# Calculate the winning bids
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep) # bids
	winners = [min(enumerate([bids[n][i] for n in range(N)]), key=itemgetter(1))[0] for i in range(w_range)] # winners
	# Find the intersection (if exists)
	# 1. Winner for w=1
	winner_w_1 = winners[-1]
	# 2. Descend through the winners list, looking for changes in the winners
	count = 0
	for winner in reversed(winners):
		if winner_w_1 != winner: break
		else: count += 1
	return w[w_range - count]


def run(n, N, pmin, pmax):
	'''This function executes n iterations of the digital marketplace
	auctioning game.
	Keyword args:
	n -- number of iterations
	N -- number of bidders
	pmin -- minimum price/cost of the service
	pmax -- maximum price/cost of the service
	'''
	# Run the simulation for n times
	# intersections = [simulate(N, pmin, pmax) for i in range(n)]
	intersections = []
	no_intersection = 0
	for i in range(n):
		sim = simulate(N, pmin, pmax)
		while(sim == 0.0):
			no_intersection += 1
			sim = simulate(N, pmin, pmax)
		intersections.append(sim)
	# Compute the mean, std and median of the distribution
	avg_intersect = np.mean(intersections)
	std_intersect = np.std(intersections)
	med_intersect = np.median(intersections)
	# Compute the probability of an intersection occurring
	prob_intersect = 1 - no_intersection/(len(intersections)+no_intersection)
	print("Probability of an intersection occurring: {0}".format(prob_intersect))
	# Plot the time series
	plt.figure()
	plt.plot(np.linspace(1, n, n), intersections, 'b*')
	plt.xlabel(r"Iterations, $n$")
	plt.ylabel(r"Intersections, $w$")
	plt.grid()
	plt.savefig("time_series.pdf")
	# Plot the histogram
	fig = plt.figure()
	ax = fig.add_subplot(111)
	h = 2*(mstats.idealfourths(intersections)[1]-mstats.idealfourths(intersections)[0])*n**(-1/3)
	ax.hist(intersections, (max(intersections)-min(intersections))/h, normed=1, facecolor='green')
	plt.xlabel(r"Intersections, $w$")
	plt.ylabel(r"Empirical PDF")
	plt.text(0.65, 1.12, \
			 "Mean: {0}\nMedian: {1}\nStd: {2}".format(avg_intersect, med_intersect, std_intersect), \
			 fontsize=13, \
			 horizontalalignment='left', \
			 verticalalignment='top', \
			 transform=ax.transAxes)
	plt.grid()
	plt.savefig("hist.pdf")
	# Plot the empirical CDF
	plt.figure()
	ecdf = sm.tools.ECDF(intersections)
	x_axis = np.linspace(min(intersections), max(intersections))
	plt.step(x_axis, ecdf(x_axis))
	plt.xlabel(r"Intersections, $w$")
	plt.ylabel(r"Empirical CDF")
	plt.grid()
	plt.savefig("ecdf.pdf")


if __name__ == '__main__':
	run(10000, 3, 0, 1)

