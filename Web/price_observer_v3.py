#!/usr/bin/python3
"""
price_observer_v3.py

Created by Jakub Konka on 2011-05-11.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import urllib.request
import smtplib
from email.mime.text import MIMEText


def fetch_new_price(address):
	try:
		# Fetch the webpage from the specified url
		response = urllib.request.urlopen(address)
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
		new_price = None
	return new_price


def fetch_old_price(file_loc):
	# Try reading from file
	try:
		f = open(file_loc, 'r')
		old_price = f.read()
		f.close()
	# If IOError, assume the file does not exist
	except IOError:
		old_price = None
	return old_price
	
	
def update_price(new_price, old_price, file_loc, link):
	try:
		if old_price is None and new_price is None:
			raise ValueError
		elif old_price is None:
			f = open(file_loc, 'w')
			f.write(new_price)
			f.close()
		elif float(new_price) < float(old_price):
			f = open(file_loc, 'w')
			f.write(new_price)
			f.close()
			send_info_message(new_price, old_price, link)
		else:
			pass
	except IOError:
		send_error_message(0)
	except TypeError:
		send_error_message(1)
	except ValueError:
		send_error_message(2)
		

def send_info_message(new_price, old_price, link):
	sender = 'jakubkonka@dias2.eee.strath.ac.uk'
	receiver = 'kubkon@gmail.com'
	msg_str = 'Price decrease on Dragon Age 2 from {0} to {1}.\nLink to the product: {2}'.format(old_price, \
				new_price, link)
	msg = MIMEText(msg_str)
	msg['Subject'] = 'Price decrease on Dragon Age 2; amazon.co.uk'
	msg['From'] = sender
	msg['To'] = receiver
	s = smtplib.SMTP('localhost')
	s.sendmail(sender, [receiver], msg.as_string())
	s.quit()
	
	
def send_error_message(error_type):
	error = {0: 'IOError', 1: 'TypeError', 2: 'ValueError'}
	sender = 'jakubkonka@dias2.eee.strath.ac.uk'
	receiver = 'kubkon@gmail.com'
	msg_str = 'A {0} has occurred.'.format(error[error_type])
	msg = MIMEText(msg_str)
	msg['Subject'] = 'Price observer error'
	msg['From'] = sender
	msg['To'] = receiver
	s = smtplib.SMTP('localhost')
	s.sendmail(sender, [receiver], msg.as_string())
	s.quit()


if __name__ == '__main__':
	if len(sys.argv) != 2:
		sys.exit()
	new_price = fetch_new_price(sys.argv[1])
	old_price = fetch_old_price('Price_observer/price.log')
	update_price(new_price, old_price, 'Price_observer/price.log', sys.argv[1])
	# old_price = fetch_old_price('price.log')
	# update_price(new_price, old_price, 'price.log', sys.argv[1])