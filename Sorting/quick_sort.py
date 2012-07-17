#!/usr/bin/env python3
# encoding: utf-8
"""
quick_sort.py

Created by Jakub Konka on 2011-11-01.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""
import sys
import random as rnd

sys.setrecursionlimit(10000)


def quick_sort(array):
	'''This function implements the standard version of the
	quick sort algorithm.
	
	Keyword arguments:
	array -- input array of integers
	
	Return: array (sorted)
	'''
	left = []
	right = []
	n = len(array)
	if n > 1:
		pivot = array[n-1]
		for i in range(n-1):
			if array[i] < pivot:
				left.append(array[i])
			else:
				right.append(array[i])
		return quick_sort(left) + [pivot] + quick_sort(right)
	else:
		return array


if __name__ == '__main__':
	n = int(sys.argv[1])
	array = [rnd.randint(1,100) for i in range(n)]
	# print("Array before sorting: ", array)
	array = quick_sort(array)
	# print("Array after sorting: ", array)

