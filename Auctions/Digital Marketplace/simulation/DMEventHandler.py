#!/usr/bin/env python
# encoding: utf-8
"""
DMEventHandler.py

Created by Jakub Konka on 2012-07-25.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os
import unittest
import numpy as np
import matplotlib.pyplot as plt

from Buyer import *

from SimulationEngine.Event import *
from SimulationEngine.EventHandler import *


class DMEventHandler(EventHandler):
  '''
  Digital Marketplace specific event handler
  '''
  # IDs of handled events
  SR_EVENT = "ServiceRequest"
  ST_EVENTS = []
  
  def __init__(self, simulation_engine):
    '''
    Constructs DMEventHandler instance
    '''
    super(DMEventHandler, self).__init__(simulation_engine)
    # Initialize buyer
    self._buyer = None
    # Initialize list of bidders
    self._bidders = []
    # Initialize service requests mean interarrival rate
    self._interarrival_rate = 0
    # Initialize service requests mean duration
    self._duration = 0
  
  @property
  def buyer(self):
    '''
    Returns Buyer instance used in the simulation
    '''
    return self._buyer
  
  @buyer.setter
  def buyer(self, buyer):
    '''
    Sets Buyer instance used in the simulation
    '''
    self._buyer = buyer
  
  @property
  def bidders(self):
    '''
    Returns list of bidders
    '''
    return self._bidders
  
  def add_bidder(self, bidder):
    '''
    Adds Bidder instance
    '''
    self._bidders += [bidder]
    # Create termination event for this bidder
    DMEventHandler.ST_EVENTS += ["Bidder{}".format(len(self._bidders))]
  
  @property
  def interarrival_rate(self):
    '''
    Returns service requests mean interarrival rate
    '''
    return self._interarrival_rate
  
  @interarrival_rate.setter
  def interarrival_rate(self, interarrival_rate):
    '''
    Sets service requests mean interarrival rate
    '''
    self._interarrival_rate = interarrival_rate
  
  @property
  def duration(self):
    '''
    Returns service requests mean duration
    '''
    return self._duration
  
  @duration.setter
  def duration(self, duration):
    '''
    Sets service requests mean duration
    '''
    self._duration = duration
  
  def _handle_start(self):
    '''
    Overriden
    '''
    self._schedule_sr_event(self._simulation_engine.simulation_time)
  
  def _handle_stop(self):
    '''
    Overriden
    '''
    # Print costs of bidders
    print("Bidder 1 cost: {}".format(self._bidders[0].cost))
    print("Bidder 2 cost: {}".format(self._bidders[1].cost))
    # Print prices paid by the buyer
    print("Prices: {}".format(self._buyer.prices))
    plt.figure()
    plt.plot(range(1, len(self._buyer.prices)+1), self._buyer.prices)
    plt.grid()
    plt.xlabel("Service requests")
    plt.ylabel("Prices")
    plt.savefig("prices.pdf")
  
  def _handle_event(self, event):
    '''
    Overriden
    '''
    print("{} : {}".format(event.time, event.identifier))
    print("Bidder 1 capacity: {} reputation: {}".format(self._bidders[0].available_capacity, self._bidders[0].reputation))
    print("Bidder 2 capacity: {} reputation: {}".format(self._bidders[1].available_capacity, self._bidders[1].reputation))
    if event.identifier == DMEventHandler.SR_EVENT:
      # Run auction
      self._run_auction(event.time)
      # Schedule next service request event
      self._schedule_sr_event(event.time)
    elif event.identifier in DMEventHandler.ST_EVENTS:
      # A bidder finished handling request
      bidder = DMEventHandler.ST_EVENTS.index(event.identifier)
      self._bidders[bidder].finish_servicing_request(Buyer.CAPACITY[self._buyer.service])
    else:
      # End of simulation event
      pass
  
  def _schedule_sr_event(self, base_time):
    '''
    Schedules next service request event
    '''
    # Calculate interarrival time
    delta_time = np.random.exponential(1 / self._interarrival_rate)
    # Create next service request event
    event = Event(DMEventHandler.SR_EVENT, base_time + delta_time)
    # Schedule the event
    self._simulation_engine.schedule(event)
  
  def _schedule_st_event(self, event_type, base_time):
    '''
    Schedules next service request termination event
    '''
    # Calculate termination time
    delta_time = np.random.exponential(self._duration)
    # Create next service termination event
    event = Event(event_type, base_time + delta_time)
    # Schedule the event
    self._simulation_engine.schedule(event)
  
  def _run_auction(self, base_time):
    '''
    Runs DM auction
    '''
    w = self._buyer.price_weight
    # Get bids from bidders
    bids = [self._bidders[0].submit_bid(w, self._bidders[1].reputation)]
    bids += [self._bidders[1].submit_bid(w, self._bidders[0].reputation)]
    # Elect the winner
    compound_bids = [w*bids[i] + (1-w)*self._bidders[i].reputation for i in range(2)]
    winner = 0
    if compound_bids[0] < compound_bids[1]:
      # Bidder 1 wins
      winner = 0
    elif compound_bids[0] > compound_bids[1]:
      # Bidder 2 wins
      winner = 1
    else:
      # Tie
      winner = np.random.randint(2)
    # Update system state
    self._buyer.add_price(bids[winner])
    self._bidders[winner].service_request(Buyer.CAPACITY[self._buyer.service])
    # Schedule termination event
    self._schedule_st_event(DMEventHandler.ST_EVENTS[winner], base_time)
  

class DMEventHandlerTests(unittest.TestCase):
  def setUp(self):
    pass
  

if __name__ == '__main__':
  unittest.main()