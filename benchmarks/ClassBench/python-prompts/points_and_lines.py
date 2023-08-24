import math

# A Point on a cartesian plane
# It is represented by a {"type": "Points", "x": <Number>,  "y": <Number>}
def Point(x, y):
    return {"type": "Point", "x": x, "y": y}


# A Line between two points
# It is represented by a {"type": "Line", "start": <Point>,  "end": <Point>}
def Line(start, end):
    return {"type": "Line", "start": start, "end": end}


# Computes the manhattan distance that a line covers
# manhattan distance = |x_1 - x_2| + |y_1 - y_2|
def manhattan_distance(l):
    # <solution>
    dx = math.fabs(l["start"]["x"] - l["end"]["x"])
    dy = math.fabs(l["start"]["y"] - l["end"]["y"])
    return dx + dy


# <tests>
def test():    
    assert (manhattan_distance(Line(Point(0, 0), Point(4, 3))) == 7)
    assert (manhattan_distance(Line(Point(1, 3), Point(4, 3))) == 3)
    assert (manhattan_distance(Line(Point(4, 2), Point(4, 3))) == 1)
    assert (manhattan_distance(Line(Point(5, 8), Point(4, 3))) == 6)
    assert (manhattan_distance(Line(Point(7, 0), Point(7, -5))) == 5)
    assert (manhattan_distance(Line(Point(4, 0), Point(-4, 0))) == 8)

test()
