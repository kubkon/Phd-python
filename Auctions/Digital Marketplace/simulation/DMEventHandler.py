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
import matplotlib.pyplot as plt

from Buyer import *

from SimulationEngine.Event import *
from SimulationEngine.EventHandler import *


class DMEventHandler(EventHandler):
  '''
  Digital Marketplace specific event handler
  '''
  def __init__(self):
    '''
    Constructs DMEventHandler instance
    '''
    super().__init__()
    ### Simulation building blocks and params
    # Initialize buyers
    self._buyers = []
    # Initialize list of bidders
    self._bidders = []
    # Initialize service requests mean interarrival rate
    self._interarrival_rate = 0
    # Initialize service requests mean duration
    self._duration = 0
    ### Output data
    # Initialize service request history list
    self._sr_history = []
    # Initialize prices paid list
    self._prices = []
    # Initialize winners list
    self._winners = []
    # Initialize reputation history dict (key: bidder)
    self._reputation_history = {}
  
  @property
  def buyers(self):
    '''
    Returns list of buyers
    '''
    return self._buyers
  
  def add_buyer(self, buyer):
    '''
    Adds Buyer instance
    '''
    self._buyers += [buyer]
  
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
    # Save results of the simulation
    self._save_results()
  
  def _handle_event(self, event):
    '''
    Overriden
    '''
    if event.identifier in self._buyers:
      # Run auction
      self._run_auction(event)
      # Schedule next service request event
      self._schedule_sr_event(event.time)
    elif event.identifier in self._bidders:
      # A bidder finished handling request
      bidder = event.identifier
      buyer = event.kwargs.get('buyer', None)
      bidder.finish_servicing_request(Buyer.CAPACITY[buyer.service])
    else:
      # End of simulation event
      pass
  
  def _schedule_sr_event(self, base_time):
    '''
    Schedules next service request event
    '''
    prng = self._simulation_engine.prng
    # Randomize through buyer types
    buyer = prng.randint(len(self._buyers))
    # Calculate interarrival time
    delta_time = prng.exponential(1 / self._interarrival_rate)
    # Create next service request event
    event = Event(self._buyers[buyer], base_time + delta_time)
    # Schedule the event
    self._simulation_engine.schedule(event)
  
  def _schedule_st_event(self, event_type, base_time, buyer):
    '''
    Schedules next service request termination event
    '''
    # Calculate termination time
    delta_time = self._simulation_engine.prng.exponential(self._duration)
    # Create next service termination event
    event = Event(event_type, base_time + delta_time, buyer=buyer)
    # Schedule the event
    self._simulation_engine.schedule(event)
  
  def _run_auction(self, event):
    '''
    Runs DM auction
    '''
    # Get price weight of current bidder type
    buyer = event.identifier
    w = buyer.price_weight
    # Get bids from bidders
    bids = [self._bidders[0].submit_bid(buyer.service, w, self._bidders[1].reputation)]
    bids += [self._bidders[1].submit_bid(buyer.service, w, self._bidders[0].reputation)]
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
      winner = self._simulation_engine.prng.randint(2)
    # Mine data: prices, winners, reputation history and service request history
    self._prices = self._prices + [bids[winner]] if self._prices else [bids[winner]]
    self._winners = self._winners + [winner] if self._winners else [winner]
    for b in self._bidders:
      self._reputation_history[b] = self._reputation_history[b] + [b.reputation] if b in self._reputation_history else [b.reputation]
    self._sr_history += [buyer] if self._sr_history else [buyer]
    # Update system state
    buyer.add_price(bids[winner])
    self._bidders[winner].service_request(Buyer.CAPACITY[buyer.service])
    # Schedule termination event
    self._schedule_st_event(self._bidders[winner], event.time, buyer)
  
  def _save_results(self):
    '''
    Saves results of the simulation
    '''
    ### Params
    # Prepare stream
    bar = "-"*50
    stream = "Bidders:\n"
    for bidder in self._bidders:
      stream += "\n{}\ncosts: {}\ncommitment: {}\n".format(bidder, bidder.costs, bidder.commitment)
    stream += bar
    stream += "\nBuyers:\n"
    for buyer in self._buyers:
      stream += "\n{}\nprice_weight: {}\nservice: {}\n".format(buyer, buyer.price_weight, buyer.service)
    # Create output directory if doesn't exist already
    dir_name = "out"
    if not os.path.exists(dir_name):
      os.makedirs(dir_name)
    # Write stream to a file
    with open(dir_name + '/params.log', mode='w', encoding='utf-8') as a_file:
      a_file.write(stream)
    ### Figures
    # Common x range
    x_range = range(1, len(self._sr_history) + 1)
    # Plot prices paid
    plt.figure()
    plt.plot(x_range, self._prices)
    plt.xlabel("Service request")
    plt.ylabel("Price")
    plt.grid()
    plt.savefig(dir_name + "/prices.pdf")
    # Plot winners
    plt.figure()
    plt.plot(x_range, self._winners, '.')
    plt.ylim([-0.5, 1.5])
    plt.xlabel("Service request")
    plt.ylabel("Winner (bidder)")
    plt.grid()
    plt.savefig(dir_name + "/winners.pdf")
    # Plot reputation history
    plt.figure()
    for b in self._bidders:
      plt.plot(x_range, self._reputation_history[b])
    plt.xlabel("Service request")
    plt.ylabel("Reputation")
    plt.legend([b for b in self._bidders], loc="upper left")
    plt.grid()
    plt.savefig(dir_name + "/reputation_history.pdf".format(b))
    # Plot service request history
    sr_history = list(map(lambda x: x.id, self._sr_history))
    plt.figure()
    plt.plot(x_range, sr_history, '.')
    plt.ylim([-0.5, max(sr_history) + 0.5])
    plt.xlabel("Service request")
    plt.ylabel("Buyer")
    plt.grid()
    plt.savefig(dir_name + "/sr_history.pdf")
  

class DMEventHandlerTests(unittest.TestCase):
  def setUp(self):
    pass
  

if __name__ == '__main__':
  unittest.main()