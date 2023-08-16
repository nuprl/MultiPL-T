-- Phone type representation using tables
local function Iphone(version)
    return { tag = "Iphone", version = version }
end

local function Android(version)
    return { tag = "Android", version = version }
end

local function Pixel(version)
    return { tag = "Pixel", version = version }
end

-- apply_to_android: Applies a function to the androids in the list of phones
local function apply_to_android(f, pList)
    -- <solution>
    local result = {}
    for _, p in ipairs(pList) do
        if p.tag == "Android" then
            table.insert(result, f(p))
        else
            table.insert(result, p)
        end
    end
    return result
end

-- <tests>
local function uppercase_android(p)
    if p.tag == "Android" then
        return Android(string.upper(p.version))
    else
        return p
    end
end

local function reverse_android(p)
    if p.tag == "Android" then
        return Android(p.version:reverse()) -- Use :reverse() method for string in Lua
    else
        return p
    end
end

local function version_android(p)
    if p.tag == "Android" then
        return Android(p.version .. "-v2")
    else
        return p
    end
end

local lu = require('luaunit')

local function tests()
    lu.assertEquals(apply_to_android(uppercase_android, {}), {})
    lu.assertEquals(
        apply_to_android(uppercase_android, {Iphone(5), Pixel(3), Android("Cupcake"), Pixel(7), Android("Grape"), Iphone(10), Android("Orange")}),
        {Iphone(5), Pixel(3), Android("CUPCAKE"), Pixel(7), Android("GRAPE"), Iphone(10), Android("ORANGE")}
    )
    lu.assertEquals(apply_to_android(reverse_android, {Iphone(5), Pixel(3), Android("Cupcake")}), {Iphone(5), Pixel(3), Android("ekacpuC")})
    lu.assertEquals(apply_to_android(version_android, {Iphone(5), Pixel(3)}), {Iphone(5), Pixel(3)})
end

tests()
