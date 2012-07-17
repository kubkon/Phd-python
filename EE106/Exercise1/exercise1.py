#!/usr/local/bin/python3
# encoding: utf-8
"""
exercise1.py

Created by Jakub Konka on 2011-09-12.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""


## Input & Output
def Hello():
	print("What's your name?")
	name = input()
	print("Hello there, {0}!".format(name))


def Repeater(n):
	string = input()
	print(n * (string + " "))
	
	
def Multiplier(n):
	num_in_str = input()
	try:
		num_in_int = int(num_in_str)
		print("{0}".format(num_in_int * n))
	except ValueError as detail:
		print("Handling runtime error: {0}".format(detail))
	except:
		print("Unexpected error occured!")
		raise


## Box Printing
def Box():
	print("+---+\n|   |\n+---+")
	
	
def WordBox():
	string = input()
	frame = "+" + (len(string)+2) * "-" + "+"
	box = frame + "\n| " + string + " |\n" + frame
	print(box)
	

## Calculator
def Add(a, b):
	return a + b
	
def Sub(a, b):
	return a - b
	
def Mul(a, b):
	return a * b
	
def Div(a, b):
	return a / b

def Mod(a, b):
	return a % b
	
def Adapt(a, b, c):
	operators = set(['+', '-', '*', '/', '%'])
	if a in operators:
		return (b, c, a)
	elif b in operators:
		return (a, c, b)
	elif c in operators:
		return (a, b, c)

def Calculator():
	try:
		num_1, num_2, op = Adapt(input(), input(), input())
		num_1, num_2 = float(num_1), float(num_2)
		if op == "+":
			result = Add(num_1, num_2)
		elif op == "-":
			result = Sub(num_1, num_2)
		elif op == "*":
			result = Mul(num_1, num_2)
		elif op == "/":
			result = Div(num_1, num_2)
		elif op == "%":
			result = Mod(num_1, num_2)
		else:
			result = "Unknown arithmetic operator."
		print(result)
	except (ValueError, TypeError) as detail:
		print("Handling runtime error: {0}".format(detail))
	except ZeroDivisionError:
		print("Division by zero!")
	except:
		print("Unknown error!")
		raise


if __name__ == '__main__':
	Hello()
	Repeater(3)
	Multiplier(5)
	Box()
	WordBox()
	Calculator()