#!/usr/bin/env python
# encoding: utf-8
"""
Buyer.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os
import unittest


class Buyer(object):
  '''
  Represents buyer in the Digital Marketplace; hence
  either end-user or service provider
  '''
  # Modeled services and capacity requirements
  WEB_BROWSING = 1
  EMAIL = 2
  CAPACITY = {WEB_BROWSING: 512, EMAIL: 256}
  
  def __init__(self, price_weight, service):
    '''
    Constructs Buyer instance
    
    Keyword arguments:
    price_weight -- Price weight requested by this buyer
    service -- Service requested by this buyer
    '''
    # Initialize prices paid array to empty
    self._prices = []
    # Assign price weight preference of the buyer
    self._price_weight = price_weight
    # Assign requested service
    self._service = service
  
  @property
  def prices(self):
    '''
    Returns prices paid array
    '''
    return self._prices
  
  def add_price(self, price):
    '''
    Adds price to the prices paid array
    
    Keyword arguments:
    price -- Price to be added
    '''
    self._prices += [price]
  
  @property
  def price_weight(self):
    '''
    Returns requested price weight of this buyer
    '''
    return self._price_weight
  
  @property
  def service(self):
    '''
    Returns service requested by this buyer
    '''
    return self._service
  

class BuyerTests(unittest.TestCase):
  def setUp(self):
    self.buyer = Buyer(0.5, Buyer.WEB_BROWSING)
  
  def test_properties(self):
    self.assertEquals(self.buyer.price_weight, 0.5)
    self.assertEquals(self.buyer.service, Buyer.WEB_BROWSING)
  
  def test_prices(self):
    [self.buyer.add_price(i) for i in range(5)]
    self.assertEquals(self.buyer.prices, [0,1,2,3,4])


if __name__ == '__main__':
  unittest.main()