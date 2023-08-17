-- A Shape can be a table like:
-- Circle: {kind="Circle", radius=float}
-- Rectangle: {kind="Rectangle", width=float, height=float}
-- Triangle: {kind="Triangle", side1=float, side2=float, side3=float}

-- Computes the perimeter of a circle
local function circle_perimeter(c)
    return 6.28 * c.radius
end

-- Computes the perimeter of a rectangle
local function rectangle_perimeter(r)
    return 2.0 * r.width + 2.0 * r.height
end

-- Computes the perimeter of a triangle
local function triangle_perimeter(t)
    return t.side1 + t.side2 + t.side3
end

-- Computes the perimeter of any Shape
local function shape_perimeter(s)
    -- <solution>
    if s.kind == "Circle" then
        return circle_perimeter(s)
    elseif s.kind == "Rectangle" then
        return rectangle_perimeter(s)
    elseif s.kind == "Triangle" then
        return triangle_perimeter(s)
    else
        error("Unknown shape type")
    end
end

-- <tests>
local lu = require('luaunit')

local function assertions()
    lu.assertEquals(shape_perimeter({kind="Circle", radius=2.0}), 12.56)
    lu.assertEquals(shape_perimeter({kind="Rectangle", width=2.0, height=4.0}), 12.0)
    lu.assertEquals(shape_perimeter({kind="Triangle", side1=3.0, side2=3.0, side3=3.0}), 9.0)
end

assertions()
