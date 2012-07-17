#!/usr/bin/env python3
# encoding: utf-8
"""
selection_sort.py

Created by Jakub Konka on 2011-11-01.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
import sys
import random as rnd


def selection_sort(array):
	'''This function implements the standard version of the
	selection sort algorithm.
	
	Keyword arguments:
	array -- input array of integers
	
	Return: None (sorted array)
	'''
	n = len(array)
	for i in range(n):
		min_el = array[i]
		min_ind = i
		for j in range(n-i-1):
			if min_el > array[i+j+1]:
				min_el = array[i+j+1]
				min_ind = i+j+1
		tmp = array[i]
		array[i] = min_el
		array[min_ind] = tmp		


if __name__ == '__main__':
	n = int(sys.argv[1])
	array = [rnd.randint(1,100) for i in range(n)]
	# print("Array before sorting: ", array)
	selection_sort(array)
	# print("Array after sorting: ", array)

