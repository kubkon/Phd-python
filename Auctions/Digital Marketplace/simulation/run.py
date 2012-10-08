#!/usr/bin/env python
# encoding: utf-8
"""
run.py

Created by Jakub Konka on 2012-10-05.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
import os
import os.path
import subprocess as sub

def main():
  ### Parse command line arguments
  parser = argparse.ArgumentParser(description="DM simulation helper script")
  parser.add_argument('reps', metavar='repetitions',
                      type=int, help='number of repetitions')
  parser.add_argument('sim_duration', metavar='simulation_duration',
                      type=int, help='duration of each simulation stage in seconds')
  parser.add_argument('--batch_size', dest='batch_size', default=4,
                      type=int, help='batch size for multiprocessing')
  args = parser.parse_args()
  repetitions = args.reps
  sim_duration = args.sim_duration
  batch_size = args.batch_size
  
  ### Run simulations
  # One process at a time
  if batch_size == 1:
    for n in range(repetitions):
      try:
        sub.call("python main.py {} --seed={} --id={}".format(sim_duration, n, n), shell=True)
      except OSError as e:
        print("Execution failed: ", e)
  # In batches
  else:
    # Split num of repetitions into batches
    quotient = repetitions // batch_size
    remainder = repetitions % batch_size
    # Run the simulations in parallel as subprocesses
    for n in range(1, quotient+2):
      num_proc = batch_size if n * batch_size <= repetitions else remainder
      procs = []
      for m in range(num_proc):
        try:
          seed = m + batch_size * (n-1)
          procs += [sub.Popen("python main.py {} --seed={} --id={}".format(sim_duration, seed, seed), shell=True)]
        except OSError as e:
          print("Execution failed: ", e)
      if procs:
        procs[-1].wait()
  
  ### Merge results from files
  # Get files (as strings)
  file_paths = (os.path.join(root, f) for root, _, files in os.walk("out") for f in files if f.endswith(".out"))
  # 


if __name__ == '__main__':
  main()
  