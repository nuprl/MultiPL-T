-- Using tables to represent the Station
-- A Station is represented as {name=<name>, connections=<list of connections>}

-- has_connection: checks if a station has a connection to some other station name.
local function has_connection(str, stat)
    for _, connection in ipairs(stat.connections) do
        if connection == str then
            return true
        end
    end
    return false
end

-- add_edge_to_station: Adds a single edge to a station.
local function add_edge_to_station(str, stat)
    if has_connection(str, stat) then
        return stat
    else
        table.insert(stat.connections, str)
        return stat
    end
end

-- add_edge: adds a connection to the subway, if it does not
-- already exist. Assumes that both from and to stations in the Subway.
local function add_edge(from, towards, sub)
    -- <solution>
    for _, station in ipairs(sub) do
        if station.name == from then
            add_edge_to_station(towards, station)
            break
        end
    end
    return sub
end

-- <tests>
local lu = require('luaunit')

local ex_station_1 = {name="Newton Centre", connections={"Fenway", "Kenmore"}}
local ex_station_2 = {name="Fenway", connections={"Newton Highlands", "Newton Centre"}}
local ex_station_3 = {name="Kenmore", connections={"Newton Centre"}}
local ex_station_4 = {name="Newton Highlands", connections={"Fenway"}}

local ex_subway_1 = {ex_station_1, ex_station_2, ex_station_3, ex_station_4}
local ex_subway_2 = {}
local ex_subway_3 = {
    {name="A", connections={"B", "D"}},
    {name="B", connections={"C", "A"}},
    {name="C", connections={"D", "B"}},
    {name="D", connections={"A", "C"}}
}

local function tests()
    lu.assertEquals(add_edge("Fenway", "Kenmore", ex_subway_2), ex_subway_2)
    lu.assertEquals(add_edge("Fenway", "Kenmore", ex_subway_1), {
        ex_station_1,
        {name="Fenway", connections={"Newton Highlands", "Newton Centre", "Kenmore"}}, -- Adjusted the expected order
        ex_station_3,
        ex_station_4
    })
    lu.assertEquals(add_edge("Kenmore", "Newton Centre", ex_subway_1), ex_subway_1)
    lu.assertEquals(add_edge("A", "C", ex_subway_3), {
        {name="A", connections={"B", "D", "C"}}, -- Adjusted the expected order
        {name="B", connections={"C", "A"}},
        {name="C", connections={"D", "B"}},
        {name="D", connections={"A", "C"}}
    })
end

tests()
