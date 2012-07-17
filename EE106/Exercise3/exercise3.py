#!/usr/bin/env python3
# encoding: utf-8
"""
exercise3.py

Created by Jakub Konka on 2011-10-28.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import random as rnd
import math


## Revision
def WC_Words(filename):
	try:
		f = open(filename,'r')
		contents = f.read().split('\n')
		f.close()
		contents = [elem for elem in contents if elem != ""]
		lines = len(contents)
		words = sum([len(elem.split()) for elem in contents])
		chars = sum([len(elem) for elem in contents])
		print("Analysing file {0}. Lines: {1} Words: {2} Characters: {3}".format(filename, lines, words, chars))
	except IOError:
		print('Cannot open file {0} for reading!'.format(filename))
	# strings = []
	# with open(filename) as f:
	# 	for line in f:
	# 		strings = strings + [line.rstrip()]
	# lines = len(strings)
	# words = 0
	# for string in strings:
	# 	words = words + len(string.split())
	# 	characters = 0
	# for string in strings:
	# 	characters = characters + len(string)
	# print("Lines:", lines, "Words:", words, "Characters:", characters)
	
def RandBox():
	size = rnd.randint(2,10)
	print("Box of size {0}.".format(size))
	edge = "+" + size*"-" + "+"
	print(edge)
	for i in range(size-2):
		print("|" + size*" " + "|")
	print(edge)
	
def RandRect():
	width = rnd.randint(2,10)
	height = rnd.randint(2,10)
	edge = "+" + width*"-" + "+"
	print(edge)
	for i in range(height):
		print("|" + width*" " + "|")
	print(edge)


## Reading text files
def Words():
	try:
		strings = []
		with open("words.txt", 'r') as f:
			for line in f:
				strings.append(line.rstrip())
		print("First word:",strings[0]," Last word:",strings[-1])
	except IOError:
		print("Cannot open file words.txt for reading!")
		
def RandomWord():
	try:
		strings = []
		with open("words.txt", 'r') as f:
			for line in f:
				strings.append(line.rstrip())
		n = rnd.randint(0,len(strings)-1)
		print("{0}th word: {1}".format(n+1,strings[n]))
	except IOError:
		print("Cannot open file words.txt for reading!")

def Censor(filename):
	try:
		words = []
		vowels = set(['a','e','i','o','u','y'])
		with open(filename, 'r') as f:
			for line in f:
				words.append(line.rstrip())
		n = rnd.randint(0,len(words)-1)
		word = []
		for letter in words[n]:
			if letter in vowels:
				word.append("*")
			else:
				word.append(letter)
		print("{0}th censored word: {1}".format(n+1,''.join(word)))
	except IOError:
		print("Cannot open file {0} for reading!".format(filename))
		
def FirstLetter():
	try:
		strings = []
		with open("words.txt", 'r') as f:
			for line in f:
				strings.append(line.rstrip())
		first_let = input("Type the first letter of a word: ")
		words = [w for w in strings if w[0] == first_let]
		print("Words starting with \"{0}\":".format(first_let))
		for w in words:
			print(w)
	except IOError:
		print("Cannot open file words.txt for reading!")


## Games
def HighLow():
	num = rnd.randint(0,100)
	user_num = int(input("Guess: "))
	while user_num != num:
		if user_num > num:
			print("Too high.")
		else:
			print("Too low.")
		user_num = int(input("Guess: "))
	print("You got it!")

def GuessWord():
	try:
		words = []
		vowels = set(['a','e','i','o','u','y'])
		with open("words.txt", 'r') as f:
			for line in f:
				words.append(line.rstrip())
		n = rnd.randint(0,len(words)-1)
		word = []
		for letter in words[n]:
			if letter in vowels:
				word.append("*")
			else:
				word.append(letter)
		print("Censored:", ''.join(word))
		user_word = input("Guess the word: ")
		while user_word != words[n]:
			print("Nope, try again.")
			user_word = input("Guess the word: ")
		print("You got it!")
	except IOError:
		print("Cannot open file words.txt for reading!")


## Code phrase
def CodePhrase():
	files = ["nouns.txt", "verbs.txt", "adjectives.txt", "nouns.txt"]
	try:
		code_frag = []
		strings = []
		for ff in files:
			with open(ff, 'r') as f:
				for line in f:
					strings.append(line.rstrip())
			n = rnd.randint(0,len(strings)-1)
			code_frag.append(strings[n])
		print("The {0} {1}s the {2} {3}.".format(code_frag[0],code_frag[1],code_frag[2],code_frag[3]))
	except IOError:
		print("Cannot open file for reading!")
		

## Hangman
def Hangman():
	try:
		words = []
		guessed = []
		with open("words.txt", 'r') as f:
			for line in f:
				words.append(line.rstrip())
		n = rnd.randint(0,len(words)-1)
		word = words[n]
		letters = [e for e in word]
		censored = ''.join(['*' for i in range(len(word))])
		tries = 0
		max_tries = 15
		while tries < max_tries and censored != word:
			print(censored)
			print(guessed)
			user_letter = input("Guess a letter or a word: ")
			guessed.append(user_letter)
			if len(user_letter) == 1:
				if user_letter not in letters:
					print("{0} is not in the word.".format(user_letter))
				else:
					while user_letter in letters:
						index = letters.index(user_letter)
						censored_list = list(censored)
						censored_list[index] = user_letter
						censored = ''.join(censored_list)
						letters[index] = '*'
			else:
				if user_letter == word:
					censored = word
				else:
					print("{0} is not the word.".format(user_letter))
			tries += 1
		print(censored)
		print(guessed)
		if tries == max_tries:
			print("You lose!")
		else:
			print("You won!")
	except IOError:
		print("Cannot open file words.txt for reading!")


## Pi
def Pi(n):
	inside = 0
	for i in range(n):
		x,y = rnd.uniform(-1,1),rnd.uniform(-1,1)
		if x**2 + y**2 < 1:
			inside += 1
	print("The approximate value of pi is: {0}".format(4 * inside/n))
	

if __name__ == '__main__':
	# files = ["adjectives.txt", "nouns.txt", "verbs.txt", "words.txt"]
	# for f in files:
	# 	WC_Words(f)
	# 	Censor(f)
	# RandBox()
	# RandRect()
	# Words()
	# RandomWord()
	# FirstLetter()
	# HighLow()
	# GuessWord()
	# CodePhrase()
	Hangman()
	# Pi(1000)

