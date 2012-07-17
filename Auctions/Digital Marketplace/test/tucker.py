#!/usr/bin/env python
# encoding: utf-8
"""
tucker.py

Created by Jakub Konka on 2011-11-12.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import numpy as np


def generate_costs(min_c, max_c):
	return [np.random.uniform(min_c, max_c) for i in range(2)]
	

def calc_w(costs, reps):
	w1 = (reps[0] - reps[1]) / ((reps[0] - reps[1]) - 2*(costs[0] - 1))
	w2 = (reps[1] - reps[0]) / ((reps[1] - reps[0]) - 2*(costs[1] - 1))
	return [w1, w2]


if __name__ == '__main__':
	costs = generate_costs(0, 1)
	reps = [0.75, 0.25]
	w = calc_w(costs, reps)
	print("Bidder 1 >>> cost: {0}, rep: {1}, w: {2}".format(costs[0], reps[0], w[0]))
	print("Bidder 2 >>> cost: {0}, rep: {1}, w: {2}".format(costs[1], reps[1], w[1]))	

