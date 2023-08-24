# Using dictionaries to represent the Station
# A Station is represented as {"name": <name>, "connections": <set of connections>}
# A Subway is a list of stations

# has_connection: checks if a station has a connection to some other station name.
def has_connection(str, stat):
    for connection in stat["connections"]:
        if connection == str:
            return True
    
    return False


# add_edge_to_station: Adds a single edge to a station.
def add_edge_to_station(str, stat):
    if has_connection(str, stat):
        return stat
    else:
        stat["connections"].add(str)
        return stat
    

# add_edge: adds a connection to the subway, if it does not
# already exist. Assumes that both from and to stations in the Subway.
def add_edge(fr, towards, sub):
    # <solution>
    for station in sub:
        if station["name"] == fr:
            add_edge_to_station(towards, station)
            break
        
    return sub


# <tests>
ex_station_1 = {"name":"Newton Centre", "connections":{"Fenway", "Kenmore"}}
ex_station_2 = {"name":"Fenway", "connections":{"Newton Highlands", "Newton Centre"}}
ex_station_3 = {"name":"Kenmore", "connections":{"Newton Centre"}}
ex_station_4 = {"name":"Newton Highlands", "connections":{"Fenway"}}

ex_subway_1 = [ex_station_1, ex_station_2, ex_station_3, ex_station_4]
ex_subway_2 = []
ex_subway_3 = [
    {"name":"A", "connections":{"B", "D"}},
    {"name":"B", "connections":{"C", "A"}},
    {"name":"C", "connections":{"D", "B"}},
    {"name":"D", "connections":{"A", "C"}}
]

def tests():
    assert(add_edge("Fenway", "Kenmore", ex_subway_2) == ex_subway_2)
    assert(add_edge("Fenway", "Kenmore", ex_subway_1) == [
        ex_station_1,
        {"name":"Fenway", "connections":{"Newton Highlands", "Newton Centre", "Kenmore"}}, # Adjusted the expected order
        ex_station_3,
        ex_station_4
    ])
    assert(add_edge("Kenmore", "Newton Centre", ex_subway_1) == ex_subway_1)
    assert(add_edge("A", "C", ex_subway_3) == [
        {"name":"A", "connections":{"B", "D", "C"}}, # Adjusted the expected order
        {"name":"B", "connections":{"C", "A"}},
        {"name":"C", "connections":{"D", "B"}},
        {"name":"D", "connections":{"A", "C"}}
    ])


tests()
