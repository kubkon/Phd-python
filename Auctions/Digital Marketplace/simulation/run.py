#!/usr/bin/env python
# encoding: utf-8
"""
run.py

Created by Jakub Konka on 2012-10-05.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
import csv
import math
import os
import os.path
import subprocess as sub


### Parse command line arguments
parser = argparse.ArgumentParser(description="DM simulation helper script")
parser.add_argument('reps', metavar='repetitions',
                    type=int, help='number of repetitions')
parser.add_argument('sim_duration', metavar='simulation_duration',
                    type=int, help='duration of each simulation stage in seconds')
parser.add_argument('--batch_size', dest='batch_size', default=4,
                    type=int, help='batch size for multiprocessing')
parser.add_argument('--save_dir', dest='save_dir', default='out',
                    help='output directory')
args = parser.parse_args()
repetitions = args.reps
sim_duration = args.sim_duration
batch_size = args.batch_size
save_dir = args.save_dir

### Run simulations
try:
  # One process at a time
  if batch_size == 1:
    for n in range(repetitions):
      sub.call("python main.py {} --seed={} --id={} --save_dir={}".format(sim_duration, n, n, save_dir), shell=True)
  # In batches
  else:
    # Split num of repetitions into batches
    quotient = repetitions // batch_size
    remainder = repetitions % batch_size
    # Run the simulations in parallel as subprocesses
    num_proc = batch_size if batch_size <= repetitions else remainder
    procs = [sub.Popen("python main.py {} --seed={} --id={} --save_dir={}".format(sim_duration, n, n, save_dir), shell=True) for n in range(num_proc)]
    while True:
      procs_poll = list(map(lambda x: x.poll() != None, procs))
      if not all(procs_poll):
        procs[procs_poll.index(False)].wait()
      elif num_proc < repetitions:
        temp_num = batch_size if num_proc + batch_size <= repetitions else remainder
        for n in range(num_proc, num_proc + temp_num):
          procs += [sub.Popen("python main.py {} --seed={} --id={} --save_dir={}".format(sim_duration, n, n, save_dir), shell=True)]
        num_proc += temp_num
      else:
        break
except OSError as e:
  print("Execution failed: ", e)

### Merge results from files
main_key = "sr_number"
# Get files (as strings)
file_paths = [os.path.join(root, f) for root, _, files in os.walk(save_dir) for f in files if f.endswith(".out")]
# Read data from files
headers = []
data_in = []
for f in file_paths:
  f_csv = csv.DictReader(open(f, 'rt'))
  f_dict = {}
  for row in f_csv:
    for key in row:
      f_dict.setdefault(key, []).append(float(row[key]))
  headers = list(f_dict.keys())
  data_in.append(f_dict)
# Reduce...
# Compute means
means = {}
for header in headers:
  if header != main_key:
    zipped = zip(*[dct[header] for dct in data_in])
    vals = list(map(lambda x: sum(x)/repetitions, zipped))
    means[header] = vals
  else:
    means[header] = data_in[0][header]
# Compute standard deviations
stds = {}
for header in headers:
  if header != main_key:
    zipped = zip(*[dct[header] for dct in data_in])
    vals = []
    for (tup, mean) in zip(zipped, means[header]):
      vals += [math.sqrt(sum(map(lambda x: (x-mean)**2, tup)) / (repetitions - 1))]
    stds[header] = vals
  else:
    stds[header] = data_in[0][header]
# Save to files
with open(save_dir + "/mean_reduced.out", 'w', newline='', encoding='utf-8') as f:
  csv_writer = csv.writer(f, delimiter=',')
  csv_writer.writerow(headers)
  for tup in zip(*[means[header] for header in headers]):
    csv_writer.writerow(tup)
with open(save_dir + "/std_reduced.out", 'w', newline='', encoding='utf-8') as f:
  csv_writer = csv.writer(f, delimiter=',')
  csv_writer.writerow(headers)
  for tup in zip(*[stds[header] for header in headers]):
    csv_writer.writerow(tup)
# Delete temporary files
for f in file_paths:
  os.remove(f)
