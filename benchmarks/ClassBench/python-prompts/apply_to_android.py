# Phone type representation using dictionaries
def Iphone(version):
    return { "tag":  "Iphone", "version": version }


def Android(version):
    return { "tag":  "Android", "version": version }


def Pixel(version):
    return { "tag":  "Pixel", "version": version }


# apply_to_android: Applies a def to the androids in the list of phones
def apply_to_android(f, pList):
    # <solution>
    result = []
    for p in pList:
        if p["tag"] == "Android":
            result.append(f(p))
        else:
            result.append(p)
        
    
    return result


# <tests>
def uppercase_android(p):
    if p["tag"] == "Android":
        return Android(p["version"].upper())
    else:
        return p
    


def reverse_android(p):
    if p["tag"] == "Android":
        return Android(p["version"][::-1])
    else:
        return p
    


def version_android(p):
    if p["tag"] == "Android":
        return Android(p["version"] + "-v2")
    else:
        return p
    
def tests():
    assert (apply_to_android(uppercase_android, []) == [])
    assert (
        apply_to_android(uppercase_android, [Iphone(5), Pixel(3), Android("Cupcake"), Pixel(7), Android("Grape"), Iphone(10), Android("Orange")]) ==
        [Iphone(5), Pixel(3), Android("CUPCAKE"), Pixel(7), Android("GRAPE"), Iphone(10), Android("ORANGE")]
    )
    assert (apply_to_android(reverse_android, [Iphone(5), Pixel(3), Android("Cupcake")]) == [Iphone(5), Pixel(3), Android("ekacpuC")])
    assert (apply_to_android(version_android, [Iphone(5), Pixel(3)]) == [Iphone(5), Pixel(3)])

tests()
