#!/usr/bin/env python3
# encoding: utf-8
"""
BitRate.py

Created by Jakub Konka on 2011-02-14.
Copyright (c) 2011 Strathclyde Uni. All rights reserved.
"""

import sys
import os
import operator
import unittest


class BitRate:
	'''Implements typical bitrate values; i.e., bps, Kbps, Mbps, Gbps, Tbps'''
	# Class tuple of possible suffixes
	SUFFIXES = ('K', 'M', 'G', 'T')
	# Class multiple value
	MULTIPLE = 1000
	
	def __init__(self, value):
		# Assign the bitrate value
		# We will use duck-typing for conversions:
		# to_numeric() and to_bitrate()
		self._bitrate = value
		
	def _operator_fallbacks(monomorphic_operator, fallback_operator):
		'''Helper method for implementing arithmetic and comparison
		operations for BitRate class'''
		def forward(a, b):
			if isinstance(b, BitRate):
				return monomorphic_operator(a, b)
			else:
				return NotImplemented
		forward.__name__ = '__' + fallback_operator.__name__ + '__'
		forward.__doc__ = monomorphic_operator.__doc__
		return forward
		
	def _add(self, other):
		'''a + b'''
		return BitRate(self._to_numeric() + other._to_numeric())
		
	__add__ = _operator_fallbacks(_add, operator.add)
			
	def _sub(self, other):
		'''a - b'''
		assert(self >= other), "Bitrate value cannot be negative!"
		return BitRate(self._to_numeric() - other._to_numeric())
		
	__sub__ = _operator_fallbacks(_sub, operator.sub)
	
	def _lt(self, other):
		'''a < b'''
		return (self._to_numeric() < other._to_numeric())
		
	__lt__ = _operator_fallbacks(_lt, operator.lt)
	
	def _le(self, other):
		'''a <= b'''
		return (self._to_numeric() <= other._to_numeric())
		
	__le__ = _operator_fallbacks(_le, operator.le)
	
	def _eq(self, other):
		'''a = b'''
		return (self._to_numeric() == other._to_numeric)
		
	__eq__ = _operator_fallbacks(_eq, operator.eq)
	
	def _gt(self, other):
		'''a > b'''
		return (self._to_numeric() > other._to_numeric())
		
	__gt__ = _operator_fallbacks(_gt, operator.gt)
	
	def _ge(self, other):
		'''a >= b'''
		return (self._to_numeric() >= other._to_numeric())
		
	__ge__ = _operator_fallbacks(_ge, operator.ge)
	
	def __int__(self):
		'''Integer representation'''
		return self._to_numeric()
	
	def __str__(self):
		'''String representation'''
		return self._to_bitrate()
		
	def _to_numeric(self):
		'''Converts bitrate into numeric representation'''
		try:
			try:
				suffix = next(b for b in self._bitrate if b.isupper())
				ind = next(i for i, v in enumerate(BitRate.SUFFIXES) if suffix == BitRate.SUFFIXES[i]) + 1
			except StopIteration:
				# Hopefully, it means that the value does not have a suffix
				suffix = ' '
				ind = 0
			num_val = int(self._bitrate.partition(suffix)[0])
			return num_val * BitRate.MULTIPLE**ind
		except TypeError:
			return self._bitrate
		except:
			print("An unexpected error occurred!")
			raise
		
	def _to_bitrate(self):
		'''Converts numeric into bitrate (string) representation'''
		try:
			size = self._bitrate
			if size < BitRate.MULTIPLE:
				return '{0}'.format(int(size))
			for suffix in BitRate.SUFFIXES:
				size /= BitRate.MULTIPLE
				if size < BitRate.MULTIPLE:
					if (lambda x: int(x) == x)(size): return '{0}{1}'.format(int(size), suffix)
					else: return '{0}{1}'.format(size, suffix)
		except TypeError:
			return self._bitrate
		except:
			print("An unexpected error occurred!")
			raise


class BitRateTests(unittest.TestCase):
		
	def setUp(self):
		self.bitrate1 = BitRate('288M')
		self.bitrate2 = BitRate(512000)
		
	def test_add(self):
		self.assertEquals((self.bitrate1 + self.bitrate2)._to_bitrate(), '288.512M')
		self.assertEquals((self.bitrate2 + self.bitrate1)._to_numeric(), 288512000)
		
	def test_sub(self):
		self.assertEquals((self.bitrate1 - self.bitrate2)._to_bitrate(), '287.488M')
		self.assertEquals((self.bitrate1 - self.bitrate2)._to_numeric(), 287488000)
		
	def test_comparisons(self):
		self.assertTrue(self.bitrate1 > self.bitrate2)
		self.assertTrue(self.bitrate1 >= self.bitrate2)
		self.assertTrue(self.bitrate2 < self.bitrate1)
		self.assertTrue(self.bitrate2 <= self.bitrate1)
		self.assertFalse(self.bitrate1 == self.bitrate2)
		self.assertFalse(self.bitrate1 <= self.bitrate2)
		self.assertFalse(self.bitrate1 < self.bitrate2)
		
	def test_to_int(self):
		self.assertTrue(isinstance(int(self.bitrate1), int))
		
	def test_to_str(self):
		self.assertTrue(isinstance(str(self.bitrate1), str))
	
	def test_to_numeric(self):
		self.assertEquals(self.bitrate1._to_numeric(), 288*10**6)
		self.assertEquals(self.bitrate2._to_numeric(), 512000)
		
	def test_to_bitrate(self):
		self.assertEquals(self.bitrate1._to_bitrate(), '288M')
		self.assertEquals(self.bitrate2._to_bitrate(), '512K')
		

if __name__ == '__main__':
	unittest.main()