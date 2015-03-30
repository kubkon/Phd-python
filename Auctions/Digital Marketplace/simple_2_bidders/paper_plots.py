#!/usr/bin/env python
# encoding: utf-8
"""
paper_plots.py

Created by Jakub Konka on 2011-08-29.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np
import scipy.integrate as integral
import matplotlib
import matplotlib.pyplot as plt
from operator import itemgetter
from matplotlib import rc


rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
## for Palatino and other serif fonts use:
#rc('font',**{'family':'serif','serif':['Palatino']))
rc('text', usetex=True)
matplotlib.rcParams.update({'font.size': 18})
	

def plot_unconstrained(types, rep):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Prices calculation
	p1 = map(lambda x: 0.5 + 0.5*types[0] - (1-x)*(rep[0]-rep[1])/(3*x), w)
	p2 = map(lambda x: 0.5 + 0.5*types[1] - (1-x)*(rep[1]-rep[0])/(3*x), w)
	prices = [p1, p2]
	## Calculation of the winning bid
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep)
	## Figure 1
	# Price evolution vs w
	plt.figure()
	plt.plot(w, prices[0], '-', linewidth=2)
	plt.plot(w, prices[1], '--', linewidth=2)
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Offered price, $b_i(c_i)$")
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
        plt.legend(legend, loc='upper right', prop={'size': 14})
        matplotlib.rcParams.update({'font.size': 14})
	plt.grid()
	plt.savefig("pincomplete_prices_uc.pdf")
	## Figure 2
	# Bids evolution vs w
	plt.figure()
	plt.plot(w, bids[0], '-', linewidth=2)
	plt.plot(w, bids[1], '--', linewidth=2)
	# plt.annotate(r"$w_c$", xy=(0.53, 0.65), xycoords='data', xytext=(15,30), textcoords='offset points', arrowprops=dict(arrowstyle="->"), fontsize=18)
	plt.annotate(r"$w_c$", xy=(0.4, 0.6), xycoords='data', xytext=(15,30), textcoords='offset points', arrowprops=dict(arrowstyle="->"), fontsize=14)
	plt.ylim([0, 1])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Compound bid, $\beta(b_i(c_i),r_i)=wb_i(c_i) + (1-w)r_i$")
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
        plt.legend(legend, loc='upper left', prop={'size': 14})
        matplotlib.rcParams.update({'font.size': 14})
	plt.grid()
	plt.savefig("pincomplete_bids_uc.pdf")
	

def plot_constrained(types, rep):
	w_range = 1000
	w = np.linspace(0.01, 1, w_range) # price weights
	## Prices calculation
	p1 = map(lambda x: max(types[0], 0.5 + 0.5*types[0] - (1-x)*(rep[0]-rep[1])/(3*x)), w)
	p2 = map(lambda x: max(types[1], 0.5 + 0.5*types[1] - (1-x)*(rep[1]-rep[0])/(3*x)), w)
	prices = [p1, p2]
	## Calculation of the winning bid
	bids = map(lambda x,y: w*x + (1-w)*y, prices, rep)
	## Figure 1
	# Price evolution vs w
	plt.figure()
	plt.plot(w, prices[0], '-', linewidth=2)
	plt.plot(w, prices[1], '--', linewidth=2)
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Offered price, $b_i^*(c_i)$")
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
	plt.legend(legend, loc='upper right')
	plt.grid()
	plt.savefig("pincomplete_prices_c.pdf")
	## Figure 2
	# Bids evolution vs w
	plt.figure()
	plt.plot(w, bids[0], '-', linewidth=2)
	plt.plot(w, bids[1], '--', linewidth=2)
	plt.annotate(r"$w_c$", xy=(0.53, 0.65), xycoords='data', xytext=(15,30), textcoords='offset points', arrowprops=dict(arrowstyle="->"), fontsize=14)
	plt.annotate(r"$w_0$", xy=(0.43, 0.625), xycoords='data', xytext=(-30,30), textcoords='offset points', arrowprops=dict(arrowstyle="->"), fontsize=14)
	plt.ylim([0, 1])
	plt.xlabel(r"Price weight, $w$")
	plt.ylabel(r"Compound bid, $\beta(b_i^*(c_i),r_i)=wb_i^*(c_i) + (1-w)r_i$")
	legend = [r"Bidder {0}".format(n+1) for n in range(2)]
	plt.legend(legend, loc='upper left')
	plt.grid()
	plt.savefig("pincomplete_bids_c.pdf")


def plot(types, rep):
	plot_unconstrained(types, rep)
	plot_constrained(types, rep)
	

if __name__ == '__main__':
	# types = [0.8440372304582047, 0.5353701861682778]
	# rep = [0.16270917969377363, 0.684543846702973]
	types = [0.75, 0.25]
	rep = [0.25, 0.75]
	plot(types, rep)
