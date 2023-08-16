-- A Point on a cartesian plane
Point = {}
Point.new = function(x, y)
    local self = {}
    self.x = x
    self.y = y
    return self
end

-- A Line between two points
Line = {}
Line.new = function(start, end_)
    local self = {}
    self.start = start
    self.end_ = end_
    return self
end

-- Computes the manhattan distance that a line covers
-- manhattan distance = |x_1 - x_2| + |y_1 - y_2|
function manhattan_distance(l)
    -- <solution>
    local dx = math.abs(l.start.x - l.end_.x)
    local dy = math.abs(l.start.y - l.end_.y)
    return dx + dy
end

-- <tests>
function assertions()
    local lu = require('luaunit')
    
    lu.assertEquals(manhattan_distance(Line.new(Point.new(0, 0), Point.new(4, 3))), 7)
    lu.assertEquals(manhattan_distance(Line.new(Point.new(1, 3), Point.new(4, 3))), 3)
    lu.assertEquals(manhattan_distance(Line.new(Point.new(4, 2), Point.new(4, 3))), 1)
    lu.assertEquals(manhattan_distance(Line.new(Point.new(5, 8), Point.new(4, 3))), 6)
    lu.assertEquals(manhattan_distance(Line.new(Point.new(7, 0), Point.new(7, -5))), 5)
    lu.assertEquals(manhattan_distance(Line.new(Point.new(4, 0), Point.new(-4, 0))), 8)
end

assertions()
