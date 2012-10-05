#!/usr/bin/env python
# encoding: utf-8
"""
run.py

Created by Jakub Konka on 2012-10-05.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
import subprocess as sub

def main():
  ### Parse command line arguments
  parser = argparse.ArgumentParser(description="DM simulation helper script")
  parser.add_argument('reps', metavar='repetitions',
                      type=int, help='number of repetitions')
  parser.add_argument('sim_duration', metavar='simulation_duration',
                      type=int, help='duration of each simulation stage in seconds')
  args = parser.parse_args()
  repetitions = args.reps
  sim_duration = args.sim_duration
  
  ### Run simulations
  # Split num of repetitions into batches of 4
  batch_size = 4
  quotient = repetitions // batch_size
  remainder = repetitions % batch_size
  # Run the simulations (4 in parallel as subprocesses)
  for n in range(1, quotient+2):
    num_proc = batch_size if n * batch_size <= repetitions else remainder
    for m in range(num_proc):
      try:
        seed = m + batch_size * (n-1)
        sub.Popen("python main.py {} --seed={} --id={}".format(sim_duration, seed, seed), shell=True)
      except OSError as e:
        print("Execution failed: ", e)


if __name__ == '__main__':
  main()