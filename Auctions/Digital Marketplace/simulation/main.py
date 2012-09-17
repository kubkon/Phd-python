#!/usr/bin/env python
# encoding: utf-8
"""
main.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys

import dm
import numpy as np
import warnings
import sim

# Neglect NumPy overflow warnings
warnings.simplefilter("ignore", RuntimeWarning)


def main():
  ### Initialize
  # Create new simulation engine
  se = sim.SimulationEngine()
  # Use NumPy PRNG with custom seed
  prng = np.random.RandomState(1000)
  se.prng = prng
  
  ### Create simulation-specific scenario
  # Create Buyers
  buyers = [dm.Buyer(0.5, dm.Buyer.WEB_BROWSING)]
  # Create Bidders
  bidders = [dm.Bidder(10000, {dm.Buyer.WEB_BROWSING: 0.5}), dm.Bidder(10000, {dm.Buyer.WEB_BROWSING: 0.5})]
  # Set commitment for both bidders
  for b in bidders:
    b.commitment = 0.5
  # Service requests mean interarrival rate (per second)
  interarrival_rate = 1
  # Service requests mean duration (in seconds)
  duration = 60*5
  # Create simulation specific event handler
  event_handler = dm.DMEventHandler()
  # Add buyers and bidders to simulation engine
  event_handler.buyers = buyers
  event_handler.bidders = bidders
  # Set params
  event_handler.interarrival_rate = interarrival_rate
  event_handler.duration = duration
  
  ### Simulate
  # Read from args list desired simulation duration
  try:
    sim_duration = int(sys.argv[1])
  except (ValueError, IndexError):
    sim_duration = 3600
  print("Simulation duration set to: {}".format(sim_duration))
  # Schedule finishing event
  se.stop(sim_duration)
  # Start simulating
  se.start()


if __name__ == '__main__':
	main()

