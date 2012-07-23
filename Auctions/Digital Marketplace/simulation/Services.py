#!/usr/bin/env python2.7
# encoding: utf-8
"""
Services.py

Created by Jakub Konka on 2012-07-23.
Copyright (c) 2012 University of Strathclyde. All rights reserved.
"""

import sys
import os


class Services:
  '''
  Enum-like class for storing mobile services,
  and their required QoS params
  '''
  # Available services
  WEB_BROWSING = 1
  EMAIL = 2
  # QoS params
  QOS_PARAMS = {1: 512, 2: 256}

