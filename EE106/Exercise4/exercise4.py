#!/usr/bin/env python3
# encoding: utf-8
"""
exercise4.py

Created by Jakub Konka on 2011-10-30.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import random as rnd
import sys

def remove_whitespaces(filename):
	try:
		lines = []
		with open(filename, 'r') as f:
			for line in f:
				lines.append(line.rstrip())
		lines = [l for l in lines if l != '']
		output = filename[0:len(filename)-4] + "_mod" + filename[len(filename)-4:len(filename)]
		with open(output, 'w') as f:
			for l in lines:
				f.write(l + "\n")
	except IOError:
		print("Cannot open file for reading/writing!")


## Revision
def Sherlock():
	try:
		lines = []
		with open("holmes.txt", 'r') as f:
			for line in f:
				lines.append(line.rstrip())
		n = rnd.randint(0, len(lines)-1)
		print(lines[n] + str("\t|Chars: ") + str(len(lines[n])))
	except IOError:
		print("Cannot open holmes.txt for reading!")
	
def Sherlock3():
	try:
		lines = []
		punct_set = set(['.',',','"','!','?',':',';','...','-','\''])
		with open("holmes.txt", 'r') as f:
			for line in f:
				lines.append(line.rstrip())
		rnd_line = rnd.randint(0, len(lines)-1)
		phrases = lines[rnd_line].split()
		rnd_phrase = rnd.randint(0, len(phrases)-1)
		phrase = phrases[rnd_phrase]
		phrase_list = list(phrase)
		word = []
		for i in range(len(phrase_list)):
			if phrase_list[i] in punct_set:
				if i > 0 and i < len(phrase_list)-1:
					if phrase_list[i-1] not in punct_set and phrase_list[i+1] not in punct_set:
						word.append(phrase_list[i])
			else:
				word.append(phrase_list[i])
		print(''.join(word))
	except IOError:
		print("Cannot open holmes.txt for reading!")


## Dictionaries
def Survey():
	animals = {}
	while True:
		animal = input("What is your favourite animal? ")
		if animal in animals:
			animals[animal] += 1
		else:
			animals[animal] = 1
		print("Survey says:")
		for a in animals:
			print(a,":",animals[a],"votes")
		
def Graph():
	ice_creams = {"chocolate": 6, "vanilla": 0, "strawberry": 4, "mint": 2}
	max_len = max([len(i) for i in ice_creams])
	for i in ice_creams:
		print(i + '.'*(max_len-len(i)) + '|' + '#'*ice_creams[i])
	
def Cockney():
	pass


## Advanced
def strip_punct(string):
	punct_set = set(['.',',','"','!','?',':',';','...','-','\''])
	chars = list(string.lower())
	word = []
	for i in range(len(chars)):
		if chars[i] in punct_set:
			if i > 0 and i < len(chars)-1:
				if chars[i-1] not in punct_set and chars[i+1] not in punct_set:
					word.append(chars[i])
		else:
			word.append(chars[i])
	return ''.join(word)

def Cloud1(filename):
	cloud_dict = {}
	with open(filename, 'r') as f:
		for line in f:
			for el in line.rstrip("\r\n").split():
				el = strip_punct(el)
				if el not in cloud_dict:
					cloud_dict[el] = 1
				else:
					cloud_dict[el] += 1
	word = input("Select a word: ")
	try:
		print("Appears: {} times".format(cloud_dict[word]))
	except KeyError:
		print("Appears: 0 times")

def Cloud2(filename):
	cloud_dict = {}
	occur_dict = {}
	with open(filename, 'r') as f:
		for line in f:
			for el in line.rstrip("\r\n").split():
				el = strip_punct(el)
				if el not in cloud_dict:
					cloud_dict[el] = 1
				else:
					cloud_dict[el] += 1
	# max_occur = max(cloud_dict.values())
	# print("Generating occurrence list for each word. Please wait...")
	# for i in range(max_occur):
	# 	occur_dict[i] = [el for el in cloud_dict if cloud_dict[el] == i]
	# 	print("{}%".format(int(i/max_occur*100)),'\r', end="")
	# 	sys.stdout.flush()
	sorted_cloud = sorted(cloud_dict.items(), key=lambda x: x[1])
	for tup in sorted_cloud:
		if tup[1] not in occur_dict:
			occur_dict[tup[1]] = [tup[0]]
		else:
			occur_dict[tup[1]].append(tup[0])
	count = int(input("Select a count: "))
	try:
		print(', '.join(occur_dict[count]))
	except KeyError:
		print("No words found.")

def Cloud3(filename):
	cloud_dict = {}
	occur_dict = {}
	with open(filename, 'r') as f:
		for line in f:
			for el in line.rstrip("\r\n").split():
				el = strip_punct(el)
				if el not in cloud_dict:
					cloud_dict[el] = 1
				else:
					cloud_dict[el] += 1
	sorted_cloud = sorted(cloud_dict.items(), key=lambda x: x[1])
	for tup in sorted_cloud:
		if tup[1] not in occur_dict:
			occur_dict[tup[1]] = [tup[0]]
		else:
			occur_dict[tup[1]].append(tup[0])
	for key in sorted(occur_dict.keys()):
		print("{} : {}".format(key, ', '.join(occur_dict[key])))
	
def Cloud4(filename):
	common_words = []
	with open("common.txt", 'r') as f:
		for line in f:
			common_words.append(line.rstrip("\r\n"))
	cloud_dict = {}
	occur_dict = {}
	with open(filename, 'r') as f:
		for line in f:
			for el in line.rstrip("\r\n").split():
				el = strip_punct(el)
				if el not in cloud_dict:
					cloud_dict[el] = 1
				else:
					cloud_dict[el] += 1
	sorted_cloud = sorted(cloud_dict.items(), key=lambda x: x[1])
	for tup in sorted_cloud:
		if tup[1] not in occur_dict:
			occur_dict[tup[1]] = [tup[0]]
		else:
			occur_dict[tup[1]].append(tup[0])
	for key in sorted(occur_dict.keys()):
		string = [el for el in occur_dict[key] if el not in common_words]
		if string:
			print("{} : {}".format(key, ', '.join(string)))

def Cloud5(filename):
	common_words = []
	with open("common.txt", 'r') as f:
		for line in f:
			common_words.append(line.rstrip("\r\n"))
	cloud_dict = {}
	occur_dict = {}
	with open(filename, 'r') as f:
		for line in f:
			for el in line.rstrip("\r\n").split():
				el = strip_punct(el)
				if el not in cloud_dict:
					cloud_dict[el] = 1
				else:
					cloud_dict[el] += 1
	sorted_cloud = sorted(cloud_dict.items(), key=lambda x: x[1])
	for tup in sorted_cloud:
		if tup[1] not in occur_dict:
			occur_dict[tup[1]] = [tup[0]]
		else:
			occur_dict[tup[1]].append(tup[0])
	counter = 0
	output = {}
	for key in sorted(occur_dict.keys(), reverse=True):
		string = [el for el in occur_dict[key] if el not in common_words]
		if string:
			if counter < 20:
				if counter == 0: max_key = key
				output[', '.join(string)] = int(key*48/max_key)
				counter += 1
			else:
				break
	ext = filename[0:len(filename)-4] + ".html"
	with open(ext, 'w') as f:
		for key in sorted(output.keys()):
			f.write("<span style=\"font-size:{}\">{}</span><br />".format(output[key], key))


if __name__ == '__main__':
	# books = ["holmes_orig.txt", "dorian_orig.txt", "jeeves_orig.txt"]
	# for b in books:
	# 	remove_whitespaces(b)
	# Sherlock()
	# Sherlock3()
	# Survey()
	# Graph()
	# Cockney()
	input_file = sys.argv[1]
	# Cloud1(input_file)
	# Cloud2(input_file)
	# Cloud3(input_file)
	# Cloud4(input_file)
	Cloud5(input_file)