-- A NumTree is either a Leaf or a Node
-- A Leaf can be represented as {type="Leaf", value=...}
-- A Node can be represented as {type="Node", value=..., left=..., right=...}

-- mirror: NumTree -> NumTree
-- Mirrors the tree around the center point
local function mirror(tree)
    -- <solution>
    if tree.type == "Leaf" then
        return {type="Leaf", value=tree.value}
    elseif tree.type == "Node" then
        return {type="Node", value=tree.value, left=mirror(tree.right), right=mirror(tree.left)}
    end
end

-- <tests>
local lu = require('luaunit')
local function assertions()
    lu.assertEquals(mirror({type="Leaf", value=5}), {type="Leaf", value=5})
    lu.assertEquals(mirror({type="Node", value=5, left={type="Leaf", value=6}, right={type="Leaf", value=7}}),
                    {type="Node", value=5, left={type="Leaf", value=7}, right={type="Leaf", value=6}})
    lu.assertEquals(mirror({type="Node", value=5, left={type="Node", value=8, left={type="Leaf", value=9}, right={type="Leaf", value=10}}, right={type="Node", value=4, left={type="Leaf", value=3}, right={type="Leaf", value=2}}}),
                    {type="Node", value=5, left={type="Node", value=4, left={type="Leaf", value=2}, right={type="Leaf", value=3}}, right={type="Node", value=8, left={type="Leaf", value=10}, right={type="Leaf", value=9}}})
end

assertions()
