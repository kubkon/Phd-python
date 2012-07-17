#!/usr/bin/env python3
# encoding: utf-8
"""
simple.py

Created by Jakub Konka on 2011-02-04.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import MobilePhone
import Mediator
import AccessTechnology
import BitRate


if __name__ == '__main__':
	# Create a mediator
	mediator = Mediator.Mediator()
	# Create mobile phones and register them with a mediator
	phones = [MobilePhone.MobilePhone(mediator) for i in range(100)]
	# Create a wifi network and register with a mediator
	wifi = AccessTechnology.AccessTechnology('WiFi', mediator)
	wifi.set_qos_params([0.1, BitRate.BitRate("54M")])
	# Create an LTE network and register with a mediator
	lte = AccessTechnology.AccessTechnology('LTE', mediator)
	lte.set_qos_params([0.5, BitRate.BitRate("1G")])
	# Create a 3G network and register with a mediator
	three_g = AccessTechnology.AccessTechnology('3G', mediator)
	three_g.set_qos_params([0.01, BitRate.BitRate('10M')])
	# Initialise network selection (sequential for now)
	for phone in (phone.select_network() for phone in phones): phone
	# Verification
	print("{0} mobile phones were created.".format(MobilePhone.MobilePhone.how_many_mobiles()))
	
	f = lambda my_id, my_list: print("\n{0} access network has been assigned the mobiles with IDs: {1}".format(my_id, my_list))
	g = lambda my_id, my_cap: print("{0} access network has {1} worth of capacity left.".format(my_id, my_cap))
	net_list = [wifi, lte, three_g]
	for net in ((f(v.network_id, v.assigned_mobiles), g(v.network_id, v.qos_params['Capacity'])) for v in net_list): net