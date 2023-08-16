-- Collatz: Number ->  Number
-- Counts how many steps it takes a number to converge
-- to 1 through the collatz sequence. The collatz sequence
-- divides by 2 if the number is even, otherwise if the number
-- is odd, it multiplies by 3 and adds 1

local function collatz(num)
    -- <solution>
    if num == 1 then
        return 0
    else
        if num % 2 == 1 then
            return 1 + collatz(num * 3 + 1)
        else
            return 1 + collatz(num / 2)
        end
    end
end

-- <tests>
local lu = require('luaunit')

local function assertions()
    lu.assertEquals(collatz(1), 0)
    lu.assertEquals(collatz(2), 1)
    lu.assertEquals(collatz(4), 2)
    lu.assertEquals(collatz(3), 7)
    lu.assertEquals(collatz(12), 9)
end

assertions()
