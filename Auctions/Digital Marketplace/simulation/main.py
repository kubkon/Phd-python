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
	net_op1 = Bidder(100000)
	net_op2 = Bidder(50000)
	# Get bids
	net_op1_bid = net_op1.submit_bid(buyer.price_weight, net_op2.reputation)
	net_op2_bid = net_op2.submit_bid(buyer.price_weight, net_op1.reputation)
	print("Bids: [{}, {}]".format(net_op1_bid, net_op2_bid))


if __name__ == '__main__':
	main()

