#!/usr/local/bin/python3
# encoding: utf-8
"""
exercise2.py

Created by Jakub Konka on 2011-09-13.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import operator as op

## Revision
def Joke():
	print("Knock knock.")
	ans1 = input()
	print("Bernadette.")
	ans2 = input()
	print("Bernadette all my dinner and now I'm starving!")
	
def Divide():
	a, b = input(), input()
	try:
		a, b = int(a), int(b)
		quo = a // b
		re = a % b
		check = quo * b
		print("{0} / {1} = {2} remainder {3}".format(a, b, quo, re))
		print("\nCheck:\n{0} * {1} = {2}".format(quo, b, check))
		print("{0} + {1} = {2}".format(check, re, check + re))
	except ValueError as detail:
		print("Handling runtime error: {0}".format(detail))
	except ZeroDivisionError:
		print("Division by zero!")
	except:
		print("Unexpected error occured!")
		raise


## Input and lists
def WordCount():
	punctuation = set(['.', ',', '!', '"', ':', ';', '\'', '?', '-', '...'])
	string = input()
	split_res = string.split()
	words = [w for w in split_res if w not in punctuation]
	print(len(words))

def Sum():
	str_list = input()
	num_list = str_list.split(',')
	num_list = [int(el) for el in num_list]
	print(sum(num_list))
	
	
## Accumulator
def Accumulator():
	num_list = []
	while True:
		num_list.append(int(input()))
		print("{0} {1}".format(num_list, sum(num_list)))

def Accumulator2():
	num_list = []
	op = [' = ']
	while True:
		num_list.append(int(input()))
		num_len = len(num_list)
		if num_len > 1:
			op.append(' + ')
		op.reverse()
		string = [str(num_list[i]) + op[i] for i in range(num_len)]
		print("{0}{1}".format(''.join(string), sum(num_list)))
		op.reverse()


## Word Count
def WC():
	string = []
	while True:
		string += [input()]
		lines = len(string)
		words = sum([len(e.split()) for e in string])
		chars = sum([len(e) for e in string])
		print("Lines: {0} Words: {1} Characters: {2}".format(lines, words, chars))
	
	
## Follow-up
def SizedBox():
	pass
	
def RPN2():
	stack = []
	dict_ops = {"+": op.add,
				"-": op.sub,
				"*": op.mul,
				"/": op.truediv,}

	while True:
		in_var = input("> ")
		if in_var not in set(dict_ops.keys()):
			stack.append(int(in_var))
		else:
			b = stack.pop()
			a = stack.pop()
			res = dict_ops[in_var](a,b)
			stack.append(res)
			print(a,in_var,b,"=",res)
	

if __name__ == '__main__':
	# Joke()
	# Divide()
	# WordCount()
	# Sum()
	# Accumulator()
	# Accumulator2()
	# WC()
	# SizedBox()
	RPN2()
