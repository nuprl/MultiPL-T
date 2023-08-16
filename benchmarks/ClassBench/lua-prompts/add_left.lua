-- Using tables to represent the NumTree
-- A Leaf is represented as {type="Leaf", value=<number>}
-- A Node is represented as {type="Node", value=<number>, left=<leftTree>, right=<rightTree>}

-- add_left: NumTree -> Number
-- Adds only the numbers on the leftmost side of the tree.
local function add_left(tree)
    -- <solution>
    if tree.type == "Leaf" then
        return tree.value
    else -- Assuming tree.type == "Node"
        return tree.value + add_left(tree.left)
    end
end

-- <tests>
local lu = require('luaunit')

local function tests()
    lu.assertEquals(add_left({type="Leaf", value=4}), 4)
    lu.assertEquals(add_left({type="Node", value=5, left={type="Leaf", value=6}, right={type="Leaf", value=7}}), 11)
    lu.assertEquals(add_left({
        type="Node", value=-3, 
        left={type="Node", value=3, left={type="Leaf", value=0}, right={type="Leaf", value=9}},
        right={type="Node", value=4, left={type="Leaf", value=9}, right={type="Leaf", value=10}}
    }), 0)
end

tests()
