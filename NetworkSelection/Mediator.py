#!/usr/bin/env python3
# encoding: utf-8
"""
Mediator.py

Created by Jakub Konka on 2011-02-07.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os
import unittest


class Mediator:
	'''An abstraction of a TCP/IP stack and physical communications channels.'''
	def __init__(self):
		# Initialise the dictionaries of existing mobiles and networks
		self.mobile_dict = {}
		self.network_dict = {}
		
	def __call__(self, mobile):
		# Mediator observes for changes in MobilePhone objects
		# Assert whether the passed value is registered with the mediator
		assert(mobile in self.mobile_dict.values()), "Mobile not registered with the mediator!"
		# Invoke the assign method of the access technology object
		net_id = mobile.assigned_network_id
		self.network_dict[net_id].assign_mobile(mobile.mobile_id)
		
	def register_mobile(self, mobile_id, mobile):
		'''Register mobile phone with the mediator'''
		self.mobile_dict[mobile_id] = mobile
		
	def register_network(self, network_id, network):
		'''Register access technology with the mediator'''
		self.network_dict[network_id] = network
		

class MediatorTests(unittest.TestCase):
	def setUp(self):
		pass


if __name__ == '__main__':
	unittest.main()