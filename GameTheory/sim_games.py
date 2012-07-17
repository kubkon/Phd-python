#!/usr/bin/env python
# encoding: utf-8
"""
sim_games.py

Created by Jakub Konka on 2011-05-21.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
from __future__ import division
import sys
import os
import numpy as np


def test_strict_dominance(matrix):
	row_set = set(tuple(row) for row in matrix)
	if max(row_set) == tuple(np.amax(matrix, axis=0)):
		return np.array(max(row_set))
	else:
		return None
	

def solve_game(game_matrix):
	# Test for strictly dominant pure strategies
	solution = test_strict_dominance(game_matrix[0])
	return solution


if __name__ == '__main__':
	# Specify game's normal form
	game_matrix = [np.array([[6, 3], [7, 4]]), np.array([[5, 9], [2, 4]])]
	# Solve the game
	solutions = solve_game(game_matrix)
	print(solutions)

