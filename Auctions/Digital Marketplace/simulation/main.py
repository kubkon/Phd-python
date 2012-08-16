#!/usr/bin/env python
# encoding: utf-8
"""
main.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os

from Buyer import *
from Bidder import *
from DMEventHandler import *

from SimulationEngine.SimulationEngine import *


def main():
  ### Create scenario
  # Create Buyers
  buyers = [Buyer(0.25, Buyer.WEB_BROWSING), Buyer(0.75, Buyer.WEB_BROWSING)]
  # Create Bidders
  bidders = [Bidder(10000, {Buyer.WEB_BROWSING: 0.5}), Bidder(5000, {Buyer.WEB_BROWSING: 0.51})]
  # Set commitment for both bidders
  for b in bidders:
    b.commitment = 0.5
  # Service requests mean interarrival rate (per second)
  interarrival_rate = 1
  # Service requests mean duration (in seconds)
  duration = 60*5
  
  ### Initialize
  # Create new simulation engine
  sim = SimulationEngine()
  # Create simulation specific event handler
  event_handler = DMEventHandler(sim)
  # Add buyers and bidders to simulation engine
  for buyer in buyers: event_handler.add_buyer(buyer)
  for bidder in bidders: event_handler.add_bidder(bidder)
  # Set params
  event_handler.interarrival_rate = interarrival_rate
  event_handler.duration = duration
  
  ### Simulate
  # Schedule finishing event
  sim.stop(60*5)
  # Start simulating
  sim.start()


if __name__ == '__main__':
	main()

