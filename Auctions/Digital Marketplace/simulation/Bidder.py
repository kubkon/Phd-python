#!/usr/bin/env python
# encoding: utf-8
"""
Bidder.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os
import unittest
import numpy as np

from Buyer import *
from NumericalToolbox import *


class Bidder(object):
  '''
  Represents bidder in the Digital Marketplace; hence
  a network operator
  '''
  # ID counter
  ID_COUNTER = 0
  
  def __init__(self, total_capacity, costs=None):
    '''
    Constructs Bidder instance
    
    Keyword arguments:
    total_capacity -- Total capacity available
    costs -- (Optional) costs per service type
    '''
    # Create ID for this instance
    self._id = Bidder.ID_COUNTER
    # Increment ID counter
    Bidder.ID_COUNTER += 1
    # Initialize costs dict (cost per service type)
    self._costs = {} if costs is None else costs
    # Initialize reputation to default value
    self._reputation = 0.5
    # Assign total capacity available to the bidder
    self._total_capacity = total_capacity
    # Initialize available capacity
    self._available_capacity = total_capacity
    # Initialize commitment
    self._commitment = 0.5
  
  def __str__(self):
    '''
    Returns string representation of the object
    '''
    return "Bidder_" + str(self._id)
  
  @property
  def id(self):
    '''
    Returns unique ID of the object
    '''
    return self._id
  
  @property
  def costs(self):
    '''
    Returns costs dict
    '''
    return self._costs
  
  @property
  def reputation(self):
    '''
    Returns current reputation of the bidder
    '''
    return self._reputation
  
  @property
  def available_capacity(self):
    '''
    Returns available capacity
    '''
    return self._available_capacity
  
  @property
  def commitment(self):
    '''
    Returns commitment (ranging from 0.0 to 1.0)
    '''
    return self._commitment
  
  @commitment.setter
  def commitment(self, commitment):
    '''
    Sets commitment (can be from 0.0 to 1.0)
    '''
    self._commitment = commitment
  
  def _generate_cost(self, service_type):
    '''
    Generates cost for each requested service type
    
    Keyword arguments:
    service_type -- Type of requested service
    '''
    # Check if service type already exists in dict
    if service_type not in self._costs:
      # Generate new cost for service type
      self._costs[service_type] = np.random.uniform(0,1)
  
  def _update_reputation(self, success_report):
    '''
    Updates reputation according to commitment and success report
    
    Keyword arguments:
    success_report -- Success report of current service request
    '''
    if success_report:
      self._reputation = self._reputation - 0.01 if self._reputation >= 0.01 else 0.0
    else:
      param = self._commitment / (100 * (1 - self._commitment))
      self._reputation = self._reputation + param if self._reputation + param <= 1.0 else 1.0
  
  def submit_bid(self, service_type, price_weight, enemy_reputation):
    '''
    Returns bid of the bidder for the specified params
    
    Keyword arguments:
    service_type -- Type of requested service
    price_weight -- Price weight requested by the buyer
    enemy_reputation -- Reputation of the other bidder
    '''
    # Generate cost for service type
    self._generate_cost(service_type)
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
    return bid
  
  def service_request(self, sr_capacity):
    '''
    Updates params as if serviced buyers service request
    
    Keyword arguments:
    sr_capacity -- Required capacity by the service
    '''
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
    '''
    Updates params when finishing servicing buyers service request
    '''
    # Update available capacity
    cap_sum = self._available_capacity + sr_capacity
    self._available_capacity = cap_sum if cap_sum < self._total_capacity else self._total_capacity
  

class BidderTests(unittest.TestCase):
  def setUp(self):
    self.bidder = Bidder(100)
  
  def test_generate_cost_v1(self):
    buyers = [Buyer(0.25, Buyer.WEB_BROWSING), Buyer(0.5, Buyer.WEB_BROWSING)]
    for buyer in buyers: self.bidder.generate_cost(buyer)
    self.assertEqual(self.bidder.costs.keys(), {Buyer.WEB_BROWSING})
  
  def test_generate_cost_v2(self):
    buyers = [Buyer(0.25, Buyer.WEB_BROWSING), Buyer(0.5, Buyer.EMAIL)]
    for buyer in buyers: self.bidder.generate_cost(buyer)
    self.assertEqual(self.bidder.costs.keys(), {Buyer.WEB_BROWSING, Buyer.EMAIL})


if __name__ == '__main__':
	unittest.main()