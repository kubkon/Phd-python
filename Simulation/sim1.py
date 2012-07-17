#!/usr/bin/env python2.7
# encoding: utf-8
"""
sim1.py

Created by Jakub Konka on 2011-04-20.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import SimPy.SimulationTrace as sim

class Car(sim.Process):
	def __init__(self, name, cc):
		sim.Process.__init__(self, name=name)
		self.cc = cc
		
	def go(self):
		print("{0} {1} Starting".format(sim.now(), self.name))
		yield sim.hold, self, 100.0
		print("{0} {1} Arrived".format(sim.now(), self.name))


if __name__ == '__main__':
	sim.initialize()
	car1 = Car("Car1", 2000)
	sim.activate(car1, car1.go(), at=6.0)
	car2 = Car("Car2", 1600)
	sim.activate(car2, car2.go())
	sim.simulate(until=200)
	print("Current time is {0}".format(sim.now()))

