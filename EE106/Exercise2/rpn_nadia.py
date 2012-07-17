import operator as op
stack = []
dictionary = {"+": op.add, "-": op.sub, "*": op.mul, "/": op.truediv, "//": op.floordiv}

while True:
    num = input("> ")
    if num not in set(dictionary):
        stack.append(int(num))
    else:
        b = stack.pop()
        a = stack.pop()
        result = dictionary[num](a,b)
        stack.append(result)
        print(a, num, b, "=", result)