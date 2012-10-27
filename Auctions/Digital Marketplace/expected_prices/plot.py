#!/usr/bin/env python
# encoding: utf-8
"""
plot.py

Created by Jakub Konka on 2012-10-27.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
from csv import DictReader
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rc

## for Palatino and other serif fonts use:
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
rc('text', usetex=True)
matplotlib.rcParams.update({'font.size': 18})

### Plot
print("Plotting the results...")
# Plot the results
plt.figure()
plt.errorbar(ws, means, yerr=ci, fmt='ro')
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price, $\bar{p}(w)$")
plt.ylim([1, 100])
plt.grid()
plt.savefig(save_dir + "/expected_prices_{}_{}.pdf".format(*reps))
# Limit the y-range
plt.figure()
plt.errorbar(ws, means, yerr=ci, fmt='ro')
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price, $\bar{p}(w)$")
plt.ylim([0, 1])
plt.grid()
plt.savefig(save_dir + "/expected_prices_{}_{}_limited.pdf".format(*reps))