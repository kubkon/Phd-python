#!/usr/bin/env python3
# encoding: utf-8
"""
Inventory.py

Created by Jakub Konka on 2011-02-09.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os


class Inventory:
	def __init__(self):
		self.observers = []
		self._product = None
		self._quantity = 0
		
	def attach(self, observer):
		self.observers.append(observer)
		
	@property
	def product(self):
		return self._product
	@product.setter
	def product(self, value):
		self._product = value
		self._update_observers()
		
	@property
	def quantity(self):
		return self._quantity
	@quantity.setter
	def quantity(self, value):
		self._quantity = value
		self._update_observers()
		
	def _update_observers(self):
		for observer in self.observers:
			observer()