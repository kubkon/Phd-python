#!/usr/bin/env python3
# encoding: utf-8
"""
insertion_sort.py

Created by Jakub Konka on 2011-11-01.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
import sys
import random as rnd


def insertion_sort(array):
	'''This function implements the standard version of the
	insertion sort algorithm.
	
	Keyword arguments:
	array -- input array of integers
	
	Returns: None (sorted array)
	'''
	n = len(array)
	for i in range(1, n):
		tmp = array[i]
		j = i - 1
		done = False
		while not done:
			if tmp < array[j]:
				array[j+1] = array[j]
				j -= 1
				if j < 0:
					done = True
			else:
				done = True
		array[j+1] = tmp


if __name__ == '__main__':
	n = int(sys.argv[1])
	array = [rnd.randint(1,100) for i in range(n)]
	# print("Array before sorting: ", array)
	insertion_sort(array)
	# print("Array after sorting: ", array)

