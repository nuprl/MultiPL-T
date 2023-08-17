-- hello : String -> String
-- Greets you hello!
local function hello(s)
    return "Hello " .. s .. "!"
end

-- goodbye : String -> String 
-- Greets you goodbye!
local function goodbye(s)
    return "Goodbye " .. s .. "!"
end

-- double_do_it : helper for hello_goodbye. Applies both functions to each item in the list 
-- and returns a new list with the output of both functions.
local function double_do_it(f, g, l)
    -- <solution>
    local result = {}
    for _, v in ipairs(l) do
        table.insert(result, f(v))
        table.insert(result, g(v))
    end
    return result
end

-- hello_goodbye : [List-of String] -> [List-of String]
-- Greets everyone hello and goodbye.
local function hello_goodbye(l)
    return double_do_it(hello, goodbye, l)
end

-- <tests>
local lu = require('luaunit')

local function assertions()
    lu.assertEquals(hello_goodbye({}), {})
    lu.assertEquals(hello_goodbye({"Alice", "Bob"}), {"Hello Alice!", "Goodbye Alice!", "Hello Bob!", "Goodbye Bob!"})
    lu.assertEquals(double_do_it(goodbye, hello, {"Alice", "Bob"}), {"Goodbye Alice!", "Hello Alice!", "Goodbye Bob!", "Hello Bob!"})
    -- Note: The commented out test in OCaml was also skipped here as Lua doesn't have native sqrt or power functions for lists of numbers.
end

assertions()
