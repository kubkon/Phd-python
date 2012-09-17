#!/usr/bin/env python
# encoding: utf-8
"""
dm.py

Created by Jakub Konka on 2012-08-22.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""
import matplotlib.pyplot as plt
import numpy as np
import os
import sim
import unittest
from itertools import cycle


class NumericalToolbox:
  @classmethod
  def estimate_bid_hat_function(cls, w, reps, granularity=1000):
    # Calculate params
    v1 = [(1-w)*reps[0], (1-w)*reps[0] + w]
    v2 = [(1-w)*reps[1], (1-w)*reps[1] + w]
    # Check whether nontrivial NE
    if (v2[1] >= v1[1]):
      if (v1[1] <= 2*v2[0] - v2[1]):
        graph_vf = np.linspace(v1[0], v1[1], granularity)
        bids = list(map(lambda x: v2[0], graph_vf))
      else:
        # Bid bounds
        b = [(4 * v1[0] * v2[0] - (v1[1] + v2[1])**2) / (4 * (v1[0] - v1[1] + v2[0] - v2[1])), (v1[1] + v2[1]) / 2]
        # Constants of integration
        c1 = ((v2[1]-v1[1])**2 + 4*(b[0]-v2[1])*(v1[0]-v1[1])) / (-2*(b[0]-b[1])*(v1[0]-v1[1])) * np.exp((v2[1]-v1[1]) / (2*(b[0]-b[1])))
        c2 = ((v1[1]-v2[1])**2 + 4*(b[0]-v1[1])*(v2[0]-v2[1])) / (-2*(b[0]-b[1])*(v2[0]-v2[1])) * np.exp((v1[1]-v2[1]) / (2*(b[0]-b[1])))
        # Inverse bid function
        vf = lambda x: v1[1] + (v2[1]-v1[1])**2 / (c1*(v2[1]+v1[1]-2*x)*np.exp((v2[1]-v1[1])/(v2[1]+v1[1]-2*x)) + 4*(v2[1]-x))
        # Sampling
        bids = np.linspace(b[0], b[1], granularity)
        graph_vf = list(map(vf, bids))
    else:
      if (v2[1] <= 2*v1[0] - v1[1]):
        graph_vf = np.linspace(v1[0], v1[1], granularity)
        bids = graph_vf
      else:
        # Bid bounds
        b = [(4 * v1[0] * v2[0] - (v1[1] + v2[1])**2) / (4 * (v1[0] - v1[1] + v2[0] - v2[1])), (v1[1] + v2[1]) / 2]
        # Constants of integration
        c1 = ((v2[1]-v1[1])**2 + 4*(b[0]-v2[1])*(v1[0]-v1[1])) / (-2*(b[0]-b[1])*(v1[0]-v1[1])) * np.exp((v2[1]-v1[1]) / (2*(b[0]-b[1])))
        c2 = ((v1[1]-v2[1])**2 + 4*(b[0]-v1[1])*(v2[0]-v2[1])) / (-2*(b[0]-b[1])*(v2[0]-v2[1])) * np.exp((v1[1]-v2[1]) / (2*(b[0]-b[1])))
        # Inverse bid functions
        vf = lambda x: v1[1] + (v2[1]-v1[1])**2 / (c1*(v2[1]+v1[1]-2*x)*np.exp((v2[1]-v1[1])/(v2[1]+v1[1]-2*x)) + 4*(v2[1]-x)) \
              if x <= b[1] else x
        # Sampling
        bids = np.linspace(b[0], v1[1], granularity)
        graph_vf = list(map(vf, bids))
    return bids, graph_vf
  

class Bidder:
  """
  Represents bidder in the Digital Marketplace; hence
  a network operator
  """
  # ID counter
  ID_COUNTER = 0
  
  def __init__(self, total_capacity, costs=None):
    """
    Constructs Bidder instance
    
    Keyword arguments:
    total_capacity -- Total capacity available
    costs -- (Optional) costs per service type
    """
    # Create ID for this instance
    self._id = Bidder.ID_COUNTER
    # Increment ID counter
    Bidder.ID_COUNTER += 1
    # Initialize costs dict (cost per service type)
    self._costs = {} if costs is None else costs
    # Initialize reputation to default value
    self._reputation = 0.5
    # Initialize reputation history list
    self._reputation_history = []
    # Initialize profit history dict (key: auction number)
    self._profit_history = {}
    # Assign total capacity available to the bidder
    self._total_capacity = total_capacity
    # Initialize available capacity
    self._available_capacity = total_capacity
    # Initialize commitment
    self._commitment = 0.8
  
  def __str__(self):
    return "Bidder_" + str(self._id)
  
  @property
  def id(self):
    """
    Returns unique ID of the object
    """
    return self._id
  
  @property
  def costs(self):
    """
    Returns costs dict
    """
    return self._costs
  
  @property
  def reputation(self):
    """
    Returns current reputation of the bidder
    """
    return self._reputation
  
  @property
  def reputation_history(self):
    """
    Returns reputation history of the bidder
    """
    return self._reputation_history
  
  @property
  def profit_history(self):
    """
    Returns profit history of the bidder
    """
    return self._profit_history
  
  @property
  def available_capacity(self):
    """
    Returns available capacity
    """
    return self._available_capacity
  
  @property
  def commitment(self):
    """
    Returns commitment (ranging from 0.0 to 1.0)
    """
    return self._commitment
  
  @commitment.setter
  def commitment(self, commitment):
    """
    Sets commitment (can be from 0.0 to 1.0)
    """
    self._commitment = commitment
  
  def _generate_cost(self, service_type):
    """
    Generates cost for each requested service type
    
    Keyword arguments:
    service_type -- Type of requested service
    """
    # Check if service type already exists in dict
    if service_type not in self._costs:
      # Get SimulationEngine instance
      se = sim.SimulationEngine()
      # Generate new cost for service type
      self._costs[service_type] = se.prng.uniform(0,1)
  
  def _update_reputation(self, success_report):
    """
    Updates reputation according to commitment and success report
    
    Keyword arguments:
    success_report -- Success report of current service request
    """
    if success_report:
      self._reputation = self._reputation - 0.01 if self._reputation >= 0.01 else 0.0
    else:
      param = self._commitment / (100 * (1 - self._commitment))
      self._reputation = self._reputation + param if self._reputation + param <= 1.0 else 1.0
  
  def submit_bid(self, service_type, price_weight, enemy_reputation):
    """
    Returns bid of the bidder for the specified params
    
    Keyword arguments:
    service_type -- Type of requested service
    price_weight -- Price weight requested by the buyer
    enemy_reputation -- Reputation of the other bidder
    """
    # Generate cost for service type
    self._generate_cost(service_type)
    # Save current reputation
    self._reputation_history += [self._reputation]
    # Submit bid
    bid = 0.0
    if price_weight != 0.0 and price_weight != 1.0 and self._reputation != enemy_reputation:
      # Estimate equilibrium bidding strategy functions (bids-hat)
      bids_hat, costs_hat = NumericalToolbox.estimate_bid_hat_function(price_weight, [self._reputation, enemy_reputation])
      # Calculate bid
      dist = list(map(lambda x: np.abs(x - ((1-price_weight)*self._reputation + self._costs[service_type]*price_weight)), costs_hat))
      bid = (bids_hat[dist.index(min(dist))] - (1-price_weight)*self._reputation) / price_weight
    elif price_weight == 0.0:
      bid = "Inf"
    else:
      # Calculate bid
      bid = (1 + self._costs[service_type]) / 2
    # Temporarily, save profit assuming a win
    self._current_profit = bid - self._costs[service_type] if price_weight != 0.0 else "Inf"
    return bid
  
  def service_request(self, sr_number, sr_capacity):
    """
    Updates params as if serviced buyer's service request
    
    Keyword arguments:
    sr_number -- Auction (SR) number
    sr_capacity -- Required capacity by the service
    """
    # Save current profit in a profit history dict
    self._profit_history[sr_number] = self._current_profit
    # Update capacity and reputation
    if self._available_capacity >= sr_capacity:
      # Update available capacity
      self._available_capacity -= sr_capacity
      # Update reputation
      self._update_reputation(True)
    else:
      # Update available capacity
      self._available_capacity = 0
      # Update reputation
      self._update_reputation(False)
  
  def finish_servicing_request(self, sr_capacity):
    """
    Updates params when finishing servicing buyers service request
    """
    # Update available capacity
    cap_sum = self._available_capacity + sr_capacity
    self._available_capacity = cap_sum if cap_sum < self._total_capacity else self._total_capacity
  

class DMEventHandler(sim.EventHandler):
  """
  Digital Marketplace specific event handler
  """
  # Event types
  SR_EVENT = "service_request"
  ST_EVENT = "service_termination"
  # Modeled services and bit-rate requirements
  WEB_BROWSING = 1
  EMAIL = 2
  BITRATES = {WEB_BROWSING: 512, EMAIL: 256}
  
  def __init__(self):
    """
    Constructs DMEventHandler instance
    """
    super().__init__()
    ### Simulation building blocks and params
    # Initialize list of bidders
    self._bidders = []
    # Initialize service requests mean interarrival rate
    self._interarrival_rate = 0
    # Initialize service requests duration
    self._duration = 0
    # Initialize service request counter
    self._sr_count = 0
  
  @property
  def bidders(self):
    """
    Returns list of bidders
    """
    return self._bidders
  
  @bidders.setter
  def bidders(self, bidders):
    """
    Adds Bidder instances
    """
    self._bidders = bidders
  
  @property
  def interarrival_rate(self):
    """
    Returns service requests mean interarrival rate
    """
    return self._interarrival_rate
  
  @interarrival_rate.setter
  def interarrival_rate(self, interarrival_rate):
    """
    Sets service requests mean interarrival rate
    """
    self._interarrival_rate = interarrival_rate
  
  @property
  def duration(self):
    """
    Returns service requests duration
    """
    return self._duration
  
  @duration.setter
  def duration(self, duration):
    """
    Sets service requests duration
    """
    self._duration = duration
  
  def _handle_start(self):
    """
    Overriden
    """
    self._schedule_sr_event(self._simulation_engine.simulation_time)
  
  def _handle_stop(self):
    """
    Overriden
    """
    # Save results of the simulation
    self._save_results()
  
  def _handle_event(self, event):
    """
    Overriden
    """
    if event.identifier == DMEventHandler.SR_EVENT:
      # Run auction
      self._run_auction(event)
      # Schedule next service request event
      self._schedule_sr_event(event.time)
    elif event.identifier == DMEventHandler.ST_EVENT:
      # A bidder finished handling request
      bidder, service_type = event.kwargs.get('bidder', None)
      bidder.finish_servicing_request(DMEventHandler.BITRATES[service_type])
    else:
      # End of simulation event
      pass
  
  def _schedule_sr_event(self, base_time):
    """
    Schedules next service request event
    """
    prng = self._simulation_engine.prng
    # Generate buyer (service type & price weight pair)
    price_weight = prng.uniform(0, 1)
    service_type = prng.choice(list(DMEventHandler.BITRATES.keys()), 1)[0]
    buyer = (price_weight, service_type)
    # Calculate interarrival time
    delta_time = prng.exponential(1 / self._interarrival_rate)
    # Create next service request event
    event = sim.Event(DMEventHandler.SR_EVENT, base_time + delta_time, buyer=buyer)
    # Schedule the event
    self._simulation_engine.schedule(event)
  
  def _schedule_st_event(self, base_time, bidder):
    """
    Schedules next service request termination event
    """
    # Create next service termination event
    event = sim.Event(DMEventHandler.ST_EVENT, base_time + self._duration, bidder=bidder)
    # Schedule the event
    self._simulation_engine.schedule(event)
  
  def _run_auction(self, event):
    """
    Runs DM auction
    """
    # Increment service request counter
    self._sr_count += 1
    # Get requested price weight and service type
    w, service_type = event.kwargs.get('buyer', None)
    # Get bids from bidders
    bids = [self._bidders[0].submit_bid(service_type, w, self._bidders[1].reputation)]
    bids += [self._bidders[1].submit_bid(service_type, w, self._bidders[0].reputation)]
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
    # Update system state
    winner = self._bidders[winner]
    winner.service_request(self._sr_count, DMEventHandler.BITRATES[service_type])
    # Schedule termination event
    self._schedule_st_event(event.time, (winner, service_type))
  
  def _save_results(self):
    """
    Saves results of the simulation
    """
    ### Params
    # Prepare stream
    bar = "-"*50
    stream = "Bidders:\n"
    for bidder in self._bidders:
      stream += "\n{}\ncosts: {}\ncommitment: {}\n".format(bidder, bidder.costs, bidder.commitment)
    stream += bar
    # Create output directory if doesn't exist already
    dir_name = "out"
    if not os.path.exists(dir_name):
      os.makedirs(dir_name)
    # Write stream to a file
    with open(dir_name + '/params.log', mode='w', encoding='utf-8') as a_file:
      a_file.write(stream)
    ### Figures
    # Create line and marker style lists
    styles = {"line": ["-", "--", "-.", ":"], "marker": ["o", "*", "v", "^", "<", ">", "1", "2", "3", "4", "p", "s", "x", "h"]}
    # Plot prices paid
    plt.figure()
    cycler = cycle(styles["line"])
    # Plot reputation history
    plt.figure()
    cycler = cycle(styles["line"])
    for b in self._bidders:
      plt.plot(range(1, self._sr_count + 1), b.reputation_history, next(cycler))
    plt.xlabel("Service request")
    plt.ylabel("Reputation")
    plt.legend([b for b in self._bidders], loc="upper center",
               bbox_to_anchor=(0.5, 1.1), fancybox=True, shadow=True)
    plt.grid()
    plt.savefig(dir_name + "/reputation_history.pdf")
    # Plot profit history for each bidder
    plt.figure()
    cycler = cycle(styles["marker"])
    for b in self._bidders:
      plt.plot(list(b.profit_history.keys()), list(b.profit_history.values()), next(cycler))
    plt.xlabel("Service request")
    plt.ylabel("Profit")
    plt.legend([b for b in self._bidders], loc="upper center",
               bbox_to_anchor=(0.5, 1.1), fancybox=True, shadow=True)
    plt.grid()
    plt.savefig(dir_name + "/profit_history.pdf")
  

class BuyerTests(unittest.TestCase):
  def setUp(self):
    self.buyer = Buyer(0.5, Buyer.WEB_BROWSING)
  
  def test_properties(self):
    self.assertEqual(self.buyer.price_weight, 0.5)
    self.assertEqual(self.buyer.service, Buyer.WEB_BROWSING)
  
  def test_prices(self):
    [self.buyer.add_price(i) for i in range(5)]
    self.assertEqual(self.buyer.prices, [0,1,2,3,4])
  

class BidderTests(unittest.TestCase):
  def setUp(self):
    self.bidder = Bidder(100)
  
  def test_generate_cost_v1(self):
    buyers = [Buyer(0.25, Buyer.WEB_BROWSING), Buyer(0.5, Buyer.WEB_BROWSING)]
    for buyer in buyers: self.bidder._generate_cost(buyer.service)
    self.assertEqual(self.bidder.costs.keys(), {Buyer.WEB_BROWSING})
  
  def test_generate_cost_v2(self):
    buyers = [Buyer(0.25, Buyer.WEB_BROWSING), Buyer(0.5, Buyer.EMAIL)]
    for buyer in buyers: self.bidder._generate_cost(buyer.service)
    self.assertEqual(self.bidder.costs.keys(), {Buyer.WEB_BROWSING, Buyer.EMAIL})
  

class DMEventHandlerTests(unittest.TestCase):
  def setUp(self):
    pass
  

if __name__ == '__main__':
  unittest.main()