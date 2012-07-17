#!/usr/bin/env python3
# encoding: utf-8
"""
bubble_sort.py

Created by Jakub Konka on 2011-11-01.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
import sys
import random as rnd
	

def bubble_sort(array):
	'''This function implements the standard version of the
	bubble sort algorithm.
	
	Keyword arguments:
	array -- input array of integers
	
	Returns: None (sorted array)
	
	'''
	swapped_flag = True
	n = len(array)
	while swapped_flag:
		swapped_flag = False
		for i in range(n):
			if i < n-1:
				if array[i] > array[i+1]:
					tmp = array[i]
					array[i] = array[i+1]
					array[i+1] = tmp
					swapped_flag = True


if __name__ == '__main__':
	n = int(sys.argv[1])
	array = [rnd.randint(1,100) for i in range(n)]
	# print("Array before sorting: ", array)
	bubble_sort(array)
	# print("Array after sorting: ", array)

