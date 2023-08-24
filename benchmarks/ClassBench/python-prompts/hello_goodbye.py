# hello : String -> String
# Greets you hello!
def hello(s):
    return "Hello " + s + "!"


# goodbye : String -> String 
# Greets you goodbye!
def goodbye(s):
    return "Goodbye " + s + "!"

# hello_goodbye : [List-of String] -> [List-of String]
# Greets everyone hello and goodbye.
def hello_goodbye(l):
    return double_do_it(hello, goodbye, l)

# double_do_it : helper for hello_goodbye. Applies both defs to each item in the list 
# and returns a new list with the output of both defs.
def double_do_it(f, g, l):
    # <solution>
    result = []
    for v in l:
        result.append(f(v))
        result.append(g(v))
    
    return result

# <tests>
import math
def test():
    assert (hello_goodbye([]) == [])
    assert (hello_goodbye(["Alice", "Bob"]) == ["Hello Alice!", "Goodbye Alice!", "Hello Bob!", "Goodbye Bob!"])
    assert (double_do_it(goodbye, hello, ["Alice", "Bob"]) == ["Goodbye Alice!", "Hello Alice!", "Goodbye Bob!", "Hello Bob!"])
    assert(double_do_it(lambda x: math.sqrt(x), lambda x: math.pow(x, 2), [4, 9, 16]) == [2, 16, 3, 81, 4, 256])

test()
