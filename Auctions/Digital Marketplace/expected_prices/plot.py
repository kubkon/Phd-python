#!/usr/bin/env python
# encoding: utf-8
"""
plot.py

Created by Jakub Konka on 2012-10-27.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
from csv import DictReader
from itertools import cycle
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import rc
import os

## for Palatino and other serif fonts use:
rc('font',**{'family':'sans-serif','sans-serif':['Helvetica']})
rc('text', usetex=True)
matplotlib.rcParams.update({'font.size': 18})

### Parse command line arguments
parser = argparse.ArgumentParser(description="Expected prices in one-shot DM auction")
parser.add_argument('save_dir', help='output directory')
args = parser.parse_args()
save_dir = args.save_dir

### Load data from files
print("Loading data from files...")
extension = '.out'
file_names = [f[:f.find(extension)] for root, _, files in os.walk(save_dir) for f in files \
              if f.endswith(extension) and 'transient' not in root and 'steady-state' not in root]
data_dct = {}
for fn in file_names:
  data_dct[fn] = {}
  with open(save_dir + '/' + fn + extension, mode='rt') as f:
    reader = DictReader(f)
    for row in reader:
      for key in row:
        data_dct[fn].setdefault(key,[]).append(float(row[key]))

### Plot
print("Plotting the results...")
# Plot the results
plt.figure()
styles = cycle(['.', ',', 'o', 's'])
legend = []
for key in data_dct:
  plt.plot(data_dct[key]['w'], data_dct[key]['mean'], next(styles))
  legend += [', '.join(key.split('_'))]
plt.xlabel(r"Price weight, $w$")
plt.ylabel(r"Average price")
plt.ylim([0, 1])
plt.legend(legend, prop={'size': 14})
plt.grid()
plt.savefig(save_dir + "/expected_prices.pdf")
