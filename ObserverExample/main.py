#!/usr/bin/env python3
# encoding: utf-8
"""
main.py

Created by Jakub Konka on 2011-02-09.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import Inventory
import Observer

if __name__ == '__main__':
	inventory = Inventory.Inventory()
	observer = Observer.Observer(inventory)
	inventory.attach(observer)
	inventory.product = "Widget"
	inventory.quantity = 10