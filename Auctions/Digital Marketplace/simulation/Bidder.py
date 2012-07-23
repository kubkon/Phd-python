#!/usr/bin/env python2.7
# encoding: utf-8
"""
Bidder.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

from __future__ import division
import sys
import os
import unittest
import numpy as np

from NumericalToolbox import *


class Bidder:
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
    self.cost = np.random.uniform(0,1)
    # Initialize reputation to default value
    self.reputation = 0.5
    # Assign total capacity available to the bidder
    self.total_capacity = total_capacity
  
  @property
  def reputation(self):
    '''
    Returns current reputation of the bidder
    '''
    return self.reputation
  
  def submit_bid(self, price_weight, enemy_reputation):
    '''
    Returns bid of the bidder for the specified params
    
    Keyword arguments:
    price_weight -- Price weight requested by the buyer
    enemy_reputation -- Reputation of the other bidder
    '''
    bid = 0.0
    if price_weight != 0.0 and price_weight != 1.0 and self.reputation != enemy_reputation:
      # Estimate equilibrium bidding strategy functions (bids-hat)
      bids_hat, costs_hat = NumericalToolbox.estimate_bid_hat_function(price_weight, [self.reputation, enemy_reputation])
      # Calculate bid
      dist = map(lambda x: np.abs(x - ((1-price_weight)*self.reputation + self.cost*price_weight)), costs_hat)
      bid = (bids_hat[dist.index(min(dist))] - (1-price_weight)*self.reputation) / price_weight
    elif price_weight == 0.0:
      bid = "Inf"
    else:
      # Calculate bid
      bid = (1 + self.cost) / 2
    return bid
  

class BidderTests(unittest.TestCase):
  def setUp(self):
    pass


if __name__ == '__main__':
	unittest.main()