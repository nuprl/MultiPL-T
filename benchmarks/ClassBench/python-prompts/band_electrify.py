# Guitar representation using dictionaries
def Guitar(brand_name, color, electric):
    return { "tag":  "Guitar", "brand_name":  brand_name, "color":  color, "electric":  electric }


# DrumKit representation using dictionaries
def DrumKit(brand_name, electric):
    return { "tag":  "DrumKit", "brand_name":  brand_name, "electric":  electric }


# Saxophone representation using dictionaries
def Saxophone(brand_name):
    return { "tag":  "Saxophone", "brand_name":  brand_name }


# Piano representation using dictionaries
def Piano(brand_name):
    return { "tag":  "Piano", "brand_name":  brand_name }


# electrify_guitar: Makes this an electric guitar!
def electrify_guitar(g):
    if g["tag"] == "Guitar":
        return Guitar(g["brand_name"], g["color"], True)
    else:
        return g
    
# electrify_drum_kit: Makes this an electric drum kit!
def electrify_drum_kit(d):
    if d["tag"] == "DrumKit":
        return DrumKit(d["brand_name"], True)
    else:
        return d

# electrify_instrument: Makes this an electric instrument!
def electrify_instrument(i):
    if i["tag"] == "Guitar":
        e_guitar = electrify_guitar(i)
        return { "tag":  "Guitar", "brand_name":  e_guitar["brand_name"], "color":  e_guitar["color"], "electric":  e_guitar["electric"] }
    elif i["tag"] == "DrumKit":
        e_drum_kit = electrify_drum_kit(i)
        return { "tag":  "DrumKit", "brand_name":  e_drum_kit["brand_name"], "electric":  e_drum_kit["electric"] }
    else:
        return i
    
# A band is either a OnePieceBand, a TwoPieceBand, or a ThreePieceBand
# A OnePieceBand can be represented as a {"tag": "OnePieceBand", "instrument": <instrument>}
# A TwoPieceBand can be represented as a {"tag": "TwoPieceBand", "instrument1": <instrument>, "instrument2": <instrument>}
# A ThreePieceBand can be represented as a {"tag": "ThreePieceBand", "instrument1": <instrument>, "instrument2": <instrument>, "instrument3": <instrument>}

# electrify_band: Makes this an electric band!
def electrify_band(b):
    # <solution>
    if b["tag"] == "OnePieceBand":
        return { "tag":  "OnePieceBand", "instrument" : electrify_instrument(b["instrument"]) }
    elif b["tag"] == "TwoPieceBand":
        return { "tag":  "TwoPieceBand", "instrument1" : electrify_instrument(b["instrument1"]), "instrument2" : electrify_instrument(b["instrument2"]) }
    elif b["tag"] == "ThreePieceBand":
        return { "tag":  "ThreePieceBand", "instrument1" : electrify_instrument(b["instrument1"]), "instrument2" : electrify_instrument(b["instrument2"]), "instrument3":  electrify_instrument(b["instrument3"]) }
    else:
        return b
    


# <tests>
def test():
    assert electrify_band({ "tag":  "OnePieceBand", "instrument": Guitar("Stratocaster", "red", True) })["instrument"]["electric"]
    
    assert electrify_band({ "tag":  "TwoPieceBand", "instrument1" : DrumKit("Korg", True), "instrument2" : DrumKit("Korg", False) })["instrument2"]["electric"]
    
    assert electrify_band({ "tag":  "ThreePieceBand", "instrument1" : Guitar("Stratocaster", "red", True), "instrument2" : DrumKit("Korg", False), "instrument3": Saxophone("Yamaha") })["instrument2"]["electric"]
        
test()
