#!/usr/bin/env python2.7
# encoding: utf-8
"""
joint_distribution.py

Created by Jakub Konka on 2011-04-17.
Copyright (c) 2011 University of Strathclyde. All rights reserved.
"""

import sys
import os
import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as p3


def main():
	max_price = 2
	# X grid
	x = np.linspace(0, max_price, num=1000)
	X = np.outer(x, np.ones(np.size(x)))
	# Y grid
	y = np.linspace(0, 1, num=1000)
	Y = np.outer(np.ones(np.size(y)), y)
	# Plotting
	# Contour plot in x & y directions
	fig1 = plt.figure()
	ax1 = p3.Axes3D(fig1)#, azim=270, elev=90)
	#ax1.plot_surface(X, Y, X * Y * 1/max_price)
	ax1.contour(X, Y, X*Y*1/max_price, zdir='z')
	ax1.contour(X, Y, X*Y*1/max_price, zdir='x')
	ax1.contour(X, Y, X*Y*1/max_price, zdir='y')
	ax1.set_xlabel('X')
	ax1.set_ylabel('Y')
	ax1.set_zlabel('Z')
	plt.show()
	
	

if __name__ == '__main__':
	main()

