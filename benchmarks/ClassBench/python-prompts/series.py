# A Computable is represented as a dictionary with two fields: 'value' and 'func'
# It represents: Computable of (X * (X -> X))
def createComputable(value, func):
    return {
        "value":  value,
        "func": func
    }


# update: Runs the computable to make the next computable in the series
def update(c):
    # <solution>
    return createComputable(c["func"](c["value"]), c["func"])


# <tests>
def add_one(x):
    return x + 1


def repeat_string(s):
    return s + s


def divide_2(x):
    return x / 2


# Test Computables
zero_add_one = createComputable(5, add_one)
one_add_one = update(zero_add_one)
two_add_one = update(one_add_one)
zero_repeat_string = createComputable("hi", repeat_string)
one_repeat_string = update(zero_repeat_string)
two_repeat_string = update(one_repeat_string)
zero_divide_2 = createComputable(40, divide_2)
one_divide_2 = update(zero_divide_2)
two_divide_2 = update(one_divide_2)

def test():
    assert (update(zero_add_one)["value"] == 6)
    assert (update(one_add_one)["value"] == 7)
    assert (update(two_add_one)["value"] == 8)
    assert (update(zero_repeat_string)["value"] == "hihi")
    assert (update(one_repeat_string)["value"] == "hihihihi")
    assert (update(two_repeat_string)["value"] == "hihihihihihihihi")
    assert (update(zero_divide_2)["value"] == 20)
    assert (update(one_divide_2)["value"] == 10)
    assert (update(two_divide_2)["value"] == 5)


# Run tests
test()
