#!/usr/bin/env python3
# encoding: utf-8
"""
AccessTechnology.py

Created by Jakub Konka on 2011-02-05.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os
import unittest
import BitRate


class AccessTechnology:
	'''Represents any wireless access technology.'''
	def __init__(self, network_id, mediator):
		# Assign network ID
		self.network_id = network_id
		# Register the network with the mediator
		self.mediator = mediator
		self.mediator.register_network(self.network_id, self)
		# Initialise the QoS parameters to zero
		self.qos_params = {'Price': 0, 'Capacity': BitRate.BitRate(0)}
		# Initialise the list of assigned mobiles
		self.assigned_mobiles = []
	
	def set_qos_params(self, qos_list):
		'''Populate QoS parameters from list'''
		self.qos_params['Price'] = qos_list[0]
		self.qos_params['Capacity'] = qos_list[1]

	def assign_mobile(self, mobile_id):
		'''Assign mobile phone to this access technology
		after network selection is performed'''
		self.assigned_mobiles.append(mobile_id)
		self.qos_params['Capacity'] -= self.mediator.mobile_dict[mobile_id].get_app_capacity()
		
		
class AccessTechnologyTests(unittest.TestCase):
	def setUp(self):
		pass


if __name__ == '__main__':
	unittest.main()