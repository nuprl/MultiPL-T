# Collatz: Number ->  Number
# Counts how many steps it takes a number to converge
# to 1 through the collatz sequence. The collatz sequence
# divides by 2 if the number is even, otherwise if the number
# is odd, it multiplies by 3 and adds 1

def collatz(num):
    # <solution>
    if num == 1:
        return 0
    else:
        if num % 2 == 1:
            return 1 + collatz(num * 3 + 1)
        else:
            return 1 + collatz(num / 2)
        
    


# <tests>
def test():
    assert (collatz(1) == 0)
    assert (collatz(2) == 1)
    assert (collatz(4) == 2)
    assert (collatz(3) == 7)
    assert (collatz(12) == 9)

test()
