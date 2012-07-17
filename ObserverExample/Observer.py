#!/usr/bin/env python3
# encoding: utf-8
"""
Observer.py

Created by Jakub Konka on 2011-02-09.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os


class Observer:
	def __init__(self, inventory):
		self.inventory = inventory
		
	def __call__(self):
		print(self.inventory.product)
		print(self.inventory.quantity)
		