-- A Computable is represented as a table with two fields: 'value' and 'func'
-- It represents: Computable of (X * (X -> X))
function createComputable(value, func)
    return {
        value = value,
        func = func
    }
end

-- update: Runs the computable to make the next computable in the series
function update(c)
    -- <solution>
    return createComputable(c.func(c.value), c.func)
end

-- <tests>
lu = require('luaunit')
function add_one(x)
    return x + 1
end

function repeat_string(s)
    return s .. s
end

function divide_2(x)
    return x / 2
end

-- Test Computables
local zero_add_one = createComputable(5, add_one)
local one_add_one = update(zero_add_one)
local two_add_one = update(one_add_one)
local zero_repeat_string = createComputable("hi", repeat_string)
local one_repeat_string = update(zero_repeat_string)
local two_repeat_string = update(one_repeat_string)
local zero_divide_2 = createComputable(40, divide_2)
local one_divide_2 = update(zero_divide_2)
local two_divide_2 = update(one_divide_2)

function testComputables()
    lu.assertEquals(update(zero_add_one).value, 6)
    lu.assertEquals(update(one_add_one).value, 7)
    lu.assertEquals(update(two_add_one).value, 8)
    lu.assertEquals(update(zero_repeat_string).value, "hihi")
    lu.assertEquals(update(one_repeat_string).value, "hihihihi")
    lu.assertEquals(update(two_repeat_string).value, "hihihihihihihihi")
    lu.assertEquals(update(zero_divide_2).value, 20)
    lu.assertEquals(update(one_divide_2).value, 10)
    lu.assertEquals(update(two_divide_2).value, 5)
end

-- Run tests
testComputables()
