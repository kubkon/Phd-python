#!/usr/bin/env python2.7
# encoding: utf-8
"""
Buyer.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os
import unittest

import Services


class Buyer:
  '''
  Represents buyer in the Digital Marketplace; hence
  either end-user or service provider
  '''
  def __init__(self, price_weight, service):
    '''
    Constructs Buyer instance
    
    Keyword arguments:
    price_weight -- Price weight requested by this buyer
    service -- Service requested by this buyer
    '''
    # Initialize prices paid array to empty
    self.prices = []
    # Assign price weight preference of the buyer
    self.price_weight = price_weight
    # Assign requested service
    self.service = service
  
  @property
  def price_weight(self):
    '''
    Returns requested price weight of this bidder
    '''
    return self.price_weight
  

class BuyerTests(unittest.TestCase):
  def setUp(self):
    pass


if __name__ == '__main__':
  unittest.main()