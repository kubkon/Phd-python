#!/usr/bin/env python
# encoding: utf-8
"""
plot.py

Created by Jakub Konka on 2012-10-08.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
from csv import DictReader
import matplotlib.pyplot as plt


### Parse command line arguments
parser = argparse.ArgumentParser(description="DM simulation plotting script")
parser.add_argument('input_dir', metavar='input_dir',
                    help='input directory')
args = parser.parse_args()
input_dir = args.input_dir

### Load data from files
mean_dct = {}
with open(input_dir + "/mean_reduced.out", 'rt') as f:
  reader = DictReader(f)
  for row in reader:
    for key in row:
      mean_dct.setdefault(key,[]).append(float(row[key]))

### Plot
# Figure 1: Reputation history
bidders = [h[len("reputation")+1:] for h in mean_dct if "reputation" in h]
plt.figure()
for b in bidders:
  plt.plot(mean_dct["sr_number"], mean_dct["reputation" + "_" + b])
plt.xlabel("Service request")
plt.ylabel("Reputation")
plt.legend(bidders)
plt.grid()
plt.savefig(input_dir + "/reputation.pdf")

# Figure 2: Prices
plt.figure()
plt.plot(mean_dct["sr_number"], mean_dct["price"], '.')
plt.xlabel("Service request")
plt.ylabel("Price")
plt.grid()
plt.savefig(input_dir + "/price.pdf")
plt.ylim([0,1])
plt.savefig(input_dir + "/price_ylim_0_1.pdf")
plt.ylim([0,5])
plt.savefig(input_dir + "/price_ylim_0_5.pdf")
plt.ylim([0,10])
plt.savefig(input_dir + "/price_ylim_0_10.pdf")
plt.ylim([0,100])
plt.savefig(input_dir + "/price_ylim_0_100.pdf")
