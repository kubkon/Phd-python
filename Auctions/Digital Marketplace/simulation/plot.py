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
import os


### Parse command line arguments
parser = argparse.ArgumentParser(description='DM simulation plotting script')
parser.add_argument('input_dir', metavar='input_dir',
                    help='input directory')
args = parser.parse_args()
input_dir = args.input_dir

### Get all file names
extension = '.out'
file_names = [f[:f.find(extension)] for _, _, files in os.walk(input_dir) for f in files if f.endswith(extension)]

### Reputation history
# Load data from files
ref_column = 'sr_number'
rep_dct = {}
for fn in filter(lambda x: 'reputation' in x, file_names):
  rep_dct[fn] = {}
  with open(input_dir + '/' + fn + extension, 'rt') as f:
    reader = DictReader(f)
    for row in reader:
      for key in row:
        val = float(row[key]) if key != ref_column else int(row[key])
        rep_dct[fn].setdefault(key,[]).append(val)
# Plot
# Figure 1..k=num of bidders: reps with confidence intervals
for key in rep_dct:
  plt.figure()
  plt.errorbar(rep_dct[key][ref_column], rep_dct[key]['mean'], yerr=rep_dct[key]['ci'], fmt='ro')
  plt.xlabel('Service request')
  plt.ylabel('Reputation')
  plt.grid()
  plt.savefig(input_dir + '/' + key + '.pdf')
# Figure k + 1: all reps same plot
plt.figure()
legend = []
for key in rep_dct:
  plt.plot(rep_dct[key][ref_column], rep_dct[key]['mean'])
  legend += [key[len('reputation_'):]]
plt.xlabel('Service request')
plt.ylabel('Reputation')
plt.legend(legend)
plt.grid()
plt.savefig(input_dir + '/reputation.pdf')
