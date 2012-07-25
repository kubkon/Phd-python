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

from NumericalToolbox import *


class Bidder(object):
  '''
  Represents bidder in the Digital Marketplace; hence
  a network operator
  '''
  def __init__(self, total_capacity):
    '''
    Constructs Bidder instance
    
    Keyword arguments:
    total_capacity -- Total capacity available
    '''
    # Generate pseudo-random cost of this bidder
    self._cost = np.random.uniform(0,1)
    # Initialize reputation to default value
    self._reputation = 0.5
    # Assign total capacity available to the bidder
    self._total_capacity = total_capacity
    # Initialize available capacity
    self._available_capacity = total_capacity
    # Initialize commitment
    self._commitment = 0.8
  
  @property
  def cost(self):
    '''
    Returns cost of the service
    '''
    return self._cost
  
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
  
  def submit_bid(self, price_weight, enemy_reputation):
    '''
    Returns bid of the bidder for the specified params
    
    Keyword arguments:
    price_weight -- Price weight requested by the buyer
    enemy_reputation -- Reputation of the other bidder
    '''
    bid = 0.0
    if price_weight != 0.0 and price_weight != 1.0 and self._reputation != enemy_reputation:
      # Estimate equilibrium bidding strategy functions (bids-hat)
      bids_hat, costs_hat = NumericalToolbox.estimate_bid_hat_function(price_weight, [self._reputation, enemy_reputation])
      # Calculate bid
      dist = list(map(lambda x: np.abs(x - ((1-price_weight)*self._reputation + self._cost*price_weight)), costs_hat))
      bid = (bids_hat[dist.index(min(dist))] - (1-price_weight)*self._reputation) / price_weight
    elif price_weight == 0.0:
      bid = "Inf"
    else:
      # Calculate bid
      bid = (1 + self._cost) / 2
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
    pass


if __name__ == '__main__':
	unittest.main()