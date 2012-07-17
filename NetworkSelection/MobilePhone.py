#!/usr/bin/env python3
# encoding: utf-8
"""
MobilePhone.py

Created by Jakub Konka on 2011-02-04.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os
import unittest
import random
import BitRate
import Mediator


class MobilePhone:
	'''An abstraction of a mobile phone'''
	# Class variable counting the number of created mobile phones
	_population = 0
	# Class dictionary of possible wireless applications
	APPS = {'Web-browsing': BitRate.BitRate("512K"),
			'Video streaming': BitRate.BitRate("5M")}
	
	@classmethod
	def how_many_mobiles(cls):
		'''Returns the number of created mobile phone objects'''
		return cls._population
	
	def __init__(self, mediator):
		# Increase the population by 1, and assign an ID to the mobile phone
		MobilePhone._population += 1
		self._mobile_id = MobilePhone._population
		# Register the mobile with the mediator
		self.mediator = mediator
		self.mediator.register_mobile(self.mobile_id, self)
		# Assign a wireless app
		self._app = self._assign_app()
		# Register the mobile as roaming free
		self._assigned_network_id = None
		
	def __del__(self):
		# Decrease the population by 1 since the object is destroyed
		MobilePhone._population -= 1
	
	@property
	def mobile_id(self):
		'''Returns the id of the mobile phone'''
		return self._mobile_id
	
	@property	
	def assigned_network_id(self):
		'''Returns the id of the network which this mobile is assigned to'''
		return self._assigned_network_id
		
	@assigned_network_id.setter
	def assigned_network_id(self, value):
		'''Sets the id of the network which this mobile is assigned to'''
		self._assigned_network_id = value
		self._notify_observer()
		
	def _notify_observer(self):
		'''Notify the observer of the assigned network id'''
		self.mediator(self)
		
	@property
	def app(self):
		'''Returns an application used by the mobile'''
		return self._app
		
	def _assign_app(self):
		'''Randomises through the keys of APPS dictionary and randomly assigns
		it to the mobile phone'''
		return random.choice([key for key in MobilePhone.APPS.keys()])
		
	def get_app_capacity(self):
		'''Returns the capacity required by an application'''
		return MobilePhone.APPS[self.app]
		
	def _select_candidates(self):
		'''Select canditate networks from among all available networks'''
		# Select only these networks which satisfy minimum bandwidth
		net_id_list = [key for key, value in self.mediator.network_dict.items() \
							if value.qos_params['Capacity'] >= self.get_app_capacity()]
		return net_id_list
		
	def select_network(self):
		'''Execute network selection'''
		# Select candidate networks
		net_id_list = self._select_candidates()
		if net_id_list:
			# Get QoS parameters from access technology objects
			price_list = (value.qos_params['Price'] \
							for key, value in self.mediator.network_dict.items() \
							if key in net_id_list)
			# Run an algorithm and select a network
			net_index = min(enumerate(price_list), key=lambda s: s[1])
			# Assign a mobile to a selected network
			self.assigned_network_id = net_id_list[net_index[0]]


class MobilePhoneTests(unittest.TestCase):
	def setUp(self):
		self.mediator = Mediator.Mediator()
		self.phones = [MobilePhone(self.mediator) for i in range(100)]
		
	def test_assign_app(self):
		count_1 = 0
		count_2 = 0
		for phone in self.phones:
		# 	print(phone.app)
			if phone.app == 'Web-browsing':
				count_1 += 1
			else:
				count_2 += 1
		print("Number of assigned 'Web-browsing' apps: {0}".format(count_1))
		print("Number of assigned 'Video streaming' apps: {0}".format(count_2))

if __name__ == '__main__':
	unittest.main()