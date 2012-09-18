#!/usr/bin/env python
# encoding: utf-8
"""
main.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import argparse
import dm
import logging
import numpy as np
import warnings
import sim
import sys

# Neglect NumPy overflow warnings
warnings.simplefilter("ignore", RuntimeWarning)


def main():
  ### Parse command line arguments
  parser = argparse.ArgumentParser(description="DM simulation toolkit")
  parser.add_argument('--log', dest='log_level', default='INFO',
                      help='set logging level (default: INFO)')
  parser.add_argument('sim_duration', metavar='simulation_duration',
                      type=int, help='simulation duration in seconds')
  args = parser.parse_args()
  log_level = args.log_level
  sim_duration = args.sim_duration
  
  ### Logging
  numeric_level = getattr(logging, log_level.upper(), 'INFO')
  if not isinstance(numeric_level, int):
    raise ValueError("Invalid log level: {}".format(log_level))
  logging.basicConfig(level=numeric_level)
  logging.info("Simulation duration set to: {}".format(sim_duration))
  
  ### Create simulation-specific scenario
  # Create Bidders
  bidders = [dm.Bidder(10000, {dm.DMEventHandler.WEB_BROWSING: 0.5, dm.DMEventHandler.EMAIL: 0.45}),
             dm.Bidder(10000, {dm.DMEventHandler.WEB_BROWSING: 0.5, dm.DMEventHandler.EMAIL: 0.45})]
  # Set commitment for both bidders
  for b in bidders:
    b.commitment = 0.5
  # Service requests mean interarrival rate (per second)
  interarrival_rate = 1
  # Service requests constant duration (in seconds)
  duration = 2.5 * 60
  
  ### Initialize
  # Create new simulation engine
  se = sim.SimulationEngine()
  # Use NumPy PRNG with custom seed
  prng = np.random.RandomState(1000)
  se.prng = prng
  # Create simulation specific event handler
  event_handler = dm.DMEventHandler()
  # Add bidders to simulation engine
  event_handler.bidders = bidders
  # Set params
  event_handler.interarrival_rate = interarrival_rate
  event_handler.duration = duration
  
  ### Simulate
  # Schedule finishing event
  se.stop(sim_duration)
  # Start simulating
  logging.info("Simulation started")
  se.start()
  logging.info("Simulation finished")


if __name__ == '__main__':
	main()

