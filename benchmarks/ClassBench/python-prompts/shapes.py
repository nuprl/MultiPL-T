# A Shape can be a table like:
# Circle: {"kind": "Circle", "radius"=<number>}
# Rectangle: {"kind": Rectangle", "width"=<number>, "height": <number>}
# Triangle: {"kind": "Triangle", "side1"=<number>, "side2": <number>, "side3": <number>}

# Computes the perimeter of a circle
def circle_perimeter(c):
    return 6.28 * c["radius"]


# Computes the perimeter of a rectangle
def rectangle_perimeter(r):
    return 2.0 * r["width"] + 2.0 * r["height"]


# Computes the perimeter of a triangle
def triangle_perimeter(t):
    return t["side1"] + t["side2"] + t["side3"]


# Computes the perimeter of any Shape
def shape_perimeter(s):
    # <solution>
    if s["kind"] == "Circle":
        return circle_perimeter(s)
    elif s["kind"] == "Rectangle":
        return rectangle_perimeter(s)
    elif s["kind"] == "Triangle":
        return triangle_perimeter(s)
    else:
        raise Exception("Unknown shape type")
    


# <tests>
def test():
    assert (shape_perimeter({"kind": "Circle", "radius": 2.0}) == 12.56)
    assert (shape_perimeter({"kind": "Rectangle", "width": 2.0, "height": 4.0}) == 12.0)
    assert (shape_perimeter({"kind": "Triangle", "side1": 3.0, "side2": 3.0, "side3": 3.0}) == 9.0)

test()
