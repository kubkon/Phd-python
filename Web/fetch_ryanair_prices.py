#!/usr/local/bin/python
# encoding: utf-8
"""
fetch_ryanair_prices.py

Created by Jakub Konka on 2011-10-8.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import re
from mechanize import Browser


br = Browser()
br.open("http://www.ryanair.com/")
for f in br.forms():
	print(f)
	
# br.select_form(nr=0)
# br.form['q'] = 'baggage'
# br.submit()
# print(br.response().read())