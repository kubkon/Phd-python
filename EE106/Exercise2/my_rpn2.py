# 1. Import operator module which contains arithmetic operators (among many others; look it up online!)
import operator as op

# 2. Create an empty stack and a dictionary for identifying the arithmetic operators
stack = []	# empty stack
dict_ops = {"+": op.add,		# op.add returns an addition operator; hence, a + b is equivalent to op.add(a,b)
			"-": op.sub,		# op.sub returns a subtraction operator; hence, a - b is equivalent to op.sub(a,b)
			"*": op.mul,		# op.mul returns a multiplication operator; hence, a * b is equivalent to op.mul(a,b)
			"/": op.truediv,}	# op.truediv returns a float division operator; hence, a / b is equivalent to op.truediv(a,b)

# 3. Looping
while True:
	in_var = input("> ") # get the input from the user
	# 4. If in_var is not an operator, assume it is a number and add to a stack
	if in_var not in set(dict_ops.keys()):
		stack.append(int(in_var))
	# 5. Otherwise, assume it is an operator and perform arithmetic operation
	else:
		b = stack.pop()
		a = stack.pop()
		res = dict_ops[in_var](a,b) # the tricky bit; here, we search through dict keys and return the corresponding value--in our case, an arithmetic operator
		stack.append(res)
		print(a,in_var,b,"=",res)

'''TO-DO:
-> analyse the code (especially, the dictionary section)
-> try to break it (and fix it!)
-> add an integer division operator // (look through the documentation of the operator module)
-> most importantly, have fun!!! :-)
'''