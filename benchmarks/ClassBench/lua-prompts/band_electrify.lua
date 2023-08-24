-- Guitar representation using tables
local function Guitar(brand_name, color, electric)
    return { tag = "Guitar", brand_name = brand_name, color = color, electric = electric }
end

-- DrumKit representation using tables
local function DrumKit(brand_name, electric)
    return { tag = "DrumKit", brand_name = brand_name, electric = electric }
end

-- Saxophone representation using tables
local function Saxophone(brand_name)
    return { tag = "Saxophone", brand_name = brand_name }
end

-- Piano representation using tables
local function Piano(brand_name)
    return { tag = "Piano", brand_name = brand_name }
end

-- electrify_guitar: Makes this an electric guitar!
local function electrify_guitar(g)
    if g.tag == "Guitar" then
        return Guitar(g.brand_name, g.color, true)
    else
        return g
    end
end

-- electrify_drum_kit: Makes this an electric drum kit!
local function electrify_drum_kit(d)
    if d.tag == "DrumKit" then
        return DrumKit(d.brand_name, true)
    else
        return d
    end
end

-- electrify_instrument: Makes this an electric instrument!
local function electrify_instrument(i)
    if i.tag == "Guitar" then
        local e_guitar = electrify_guitar(i)
        return { tag = "Guitar", brand_name = e_guitar.brand_name, color = e_guitar.color, electric = e_guitar.electric }
    elseif i.tag == "DrumKit" then
        local e_drum_kit = electrify_drum_kit(i)
        return { tag = "DrumKit", brand_name = e_drum_kit.brand_name, electric = e_drum_kit.electric }
    else
        return i
    end
end

-- A band is either a OnePieceBand, a TwoPieceBand, or a ThreePieceBand
-- A OnePieceBand can be represented as a {tag = "OnePieceBand", instrument = <instrument>}
-- A TwoPieceBand can be represented as a {tag = "TwoPieceBand", instrument1 = <instrument>, instrument2 = <instrument>}
-- A ThreePieceBand can be represented as a {tag = "ThreePieceBand", instrument1 = <instrument>, instrument2 = <instrument>, instrument3 = <instrument>}

-- electrify_band: Makes this an electric band!
local function electrify_band(b)
    -- <solution>
    if b.tag == "OnePieceBand" then
        return { tag = "OnePieceBand", instrument = electrify_instrument(b.instrument) }
    elseif b.tag == "TwoPieceBand" then
        return { tag = "TwoPieceBand", instrument1 = electrify_instrument(b.instrument1), instrument2 = electrify_instrument(b.instrument2) }
    elseif b.tag == "ThreePieceBand" then
        return { tag = "ThreePieceBand", instrument1 = electrify_instrument(b.instrument1), instrument2 = electrify_instrument(b.instrument2), instrument3 = electrify_instrument(b.instrument3) }
    else
        return b
    end
end

-- <tests>
lu = require('luaunit')
local function assertions()
    lu.assertEquals(
        electrify_band({ tag = "OnePieceBand", instrument = Guitar("Stratocaster", "red", true) }).instrument.electric,
        true
    )
    lu.assertEquals(
        electrify_band({ tag = "TwoPieceBand", instrument1 = DrumKit("Korg", true), instrument2 = DrumKit("Korg", false) }).instrument2.electric,
        true
    )
    lu.assertEquals(
        electrify_band({ tag = "ThreePieceBand", instrument1 = Guitar("Stratocaster", "red", true), instrument2 = DrumKit("Korg", false), instrument3 = Saxophone("Yamaha") }).instrument2.electric,
        true
    )
end

assertions()
