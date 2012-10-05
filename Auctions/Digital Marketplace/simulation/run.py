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
  script = "main.py"
  arguments = "{}".format(sim_duration)
  for n in range(repetitions):
    try:
      sub.call("python " + script + " " + arguments, shell=True)
    except OSError:
      print("Execution failed: ", OSError)


if __name__ == '__main__':
  main()