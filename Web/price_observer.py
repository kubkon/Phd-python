#!/usr/local/bin/python3
# encoding: utf-8
"""
price_observer.py

Created by Jakub Konka on 2011-05-10.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import urllib.request
import tkinter.messagebox as msg


if len(sys.argv) != 2:
	sys.exit("Must provide exactly one argument.")
	
try:
	# Fetch the webpage from the specified url
	response = urllib.request.urlopen(sys.argv[1])
	html = response.read()
	# Convert to string
	html_2_str = repr(html)
	# Search for the price specimen
	start_str = '<b class="priceLarge">'
	end_str = '</b>'
	start_ind = html_2_str.find(start_str)
	end_ind = html_2_str[start_ind:].find(end_str)
	result_str = html_2_str[start_ind:start_ind+end_ind+len(end_str)]
	# Extract the price
	price_ind = result_str.find('\\xa3')+len('\\xa3')
	dot_ind = result_str.find('.')
	new_price = result_str[price_ind:dot_ind+3]
	response.close()
except ValueError:
	msg.showerror("Error", "Must provide a valid, full http address.")
	sys.exit()

# Try reading from file
try:
	f = open('price.log', 'r')
	old_price = f.read()
	f.close()
# If IOError, assume the file does not exist
# and create a new one
except IOError:
	old_price = None
	f = open('price.log', 'w')
	f.write(new_price)
	f.close()
	sys.exit()
	
# Check if updating is needed
# and update the file
if float(new_price) < float(old_price):
	try:
		f = open('price.log', 'w')
		f.write(new_price)
		f.close()
		msg.showinfo("Price update", "Price decreased from \xa3{0} to \xa3{1}".format(old_price, new_price))
	except IOError:
		msg.showerror("Error", "Cannot open file for writing.")