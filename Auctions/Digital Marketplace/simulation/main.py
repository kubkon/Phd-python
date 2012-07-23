#!/usr/bin/env python2.7
# encoding: utf-8
"""
main.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os

from Buyer import *
from Bidder import *
from Services import *


def main():
	# Create Buyer
	buyer = Buyer(0.5, Services.WEB_BROWSING)
	# Create Bidders
	bidders = [Bidder(100000), Bidder(50000)]
	# Get bids
	bids = [bidders[0].submit_bid(buyer.price_weight, bidders[1].reputation)]
	bids += [bidders[1].submit_bid(buyer.price_weight, bidders[0].reputation)]
	print("Bids: {}".format(bids))
	# Elect the winner
	compound_bids = [buyer.price_weight*bids[i] + (1-buyer.price_weight)*bidders[i].reputation for i in range(2)]
	print("Compound bids: {}".format(compound_bids))
	if compound_bids[0] < compound_bids[1]:
	  # Bidder 1 wins
	  buyer.add_price(bids[0])
	elif compound_bids[1] > compound_bids[0]:
	  # Bidder 2 wins
	  buyer.add_price(bids[1])
	else:
	  # Tie
	  winner = np.random.randint(2)
	  buyer.add_price(bids[winner])


if __name__ == '__main__':
	main()

