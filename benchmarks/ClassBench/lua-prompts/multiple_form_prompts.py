[
  {
    "name": "band_electrify",
    "language": "lua",
    "prompt": "-- Guitar representation using tables\nlocal function Guitar(brand_name, color, electric)\n    return { tag = \"Guitar\", brand_name = brand_name, color = color, electric = electric }\nend\n\n-- DrumKit representation using tables\nlocal function DrumKit(brand_name, electric)\n    return { tag = \"DrumKit\", brand_name = brand_name, electric = electric }\nend\n\n-- Saxophone representation using tables\nlocal function Saxophone(brand_name)\n    return { tag = \"Saxophone\", brand_name = brand_name }\nend\n\n-- Piano representation using tables\nlocal function Piano(brand_name)\n    return { tag = \"Piano\", brand_name = brand_name }\nend\n\n-- electrify_guitar: Makes this an electric guitar!\nlocal function electrify_guitar(g)\n    if g.tag == \"Guitar\" then\n        return Guitar(g.brand_name, g.color, true)\n    else\n        return g\n    end\nend\n\n-- electrify_drum_kit: Makes this an electric drum kit!\nlocal function electrify_drum_kit(d)\n    if d.tag == \"DrumKit\" then\n        return DrumKit(d.brand_name, true)\n    else\n        return d\n    end\nend\n\n-- electrify_instrument: Makes this an electric instrument!\nlocal function electrify_instrument(i)\n    if i.tag == \"Guitar\" then\n        local e_guitar = electrify_guitar(i)\n        return { tag = \"Guitar\", brand_name = e_guitar.brand_name, color = e_guitar.color, electric = e_guitar.electric }\n    elseif i.tag == \"DrumKit\" then\n        local e_drum_kit = electrify_drum_kit(i)\n        return { tag = \"DrumKit\", brand_name = e_drum_kit.brand_name, electric = e_drum_kit.electric }\n    else\n        return i\n    end\nend\n\n-- electrify_band: Makes this an electric band!\nlocal function electrify_band(b)",
    "doctests": "original",
    "original": "lua-prompts/band_electrify.lua",
    "prompt_terminology": "verbatim",
    "tests": "lu = require('luaunit')\nlocal function assertions()\n    lu.assertEquals(\n        electrify_band({ tag = \"OnePieceBand\", instrument = Guitar(\"Stratocaster\", \"red\", true) }).instrument.electric,\n        true\n    )\n    lu.assertEquals(\n        electrify_band({ tag = \"TwoPieceBand\", instrument1 = DrumKit(\"Korg\", true), instrument2 = DrumKit(\"Korg\", false) }).instrument2.electric,\n        true\n    )\n    lu.assertEquals(\n        electrify_band({ tag = \"ThreePieceBand\", instrument1 = Guitar(\"Stratocaster\", \"red\", true), instrument2 = DrumKit(\"Korg\", false), instrument3 = Saxophone(\"Yamaha\") }).instrument2.electric,\n        true\n    )\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "add_station",
    "language": "lua",
    "prompt": "-- Using tables to represent the Station\n-- A Station is represented as {name=<name>, connections=<list of connections>}\n\n-- has_connection: checks if a station has a connection to some other station name.\nlocal function has_connection(str, stat)\n    for _, connection in ipairs(stat.connections) do\n        if connection == str then\n            return true\n        end\n    end\n    return false\nend\n\n-- add_edge_to_station: Adds a single edge to a station.\nlocal function add_edge_to_station(str, stat)\n    if has_connection(str, stat) then\n        return stat\n    else\n        table.insert(stat.connections, str)\n        return stat\n    end\nend\n\n-- add_edge: adds a connection to the subway, if it does not\n-- already exist. Assumes that both from and to stations in the Subway.\nlocal function add_edge(from, towards, sub)",
    "doctests": "original",
    "original": "lua-prompts/add_station.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\n\nlocal ex_station_1 = {name=\"Newton Centre\", connections={\"Fenway\", \"Kenmore\"}}\nlocal ex_station_2 = {name=\"Fenway\", connections={\"Newton Highlands\", \"Newton Centre\"}}\nlocal ex_station_3 = {name=\"Kenmore\", connections={\"Newton Centre\"}}\nlocal ex_station_4 = {name=\"Newton Highlands\", connections={\"Fenway\"}}\n\nlocal ex_subway_1 = {ex_station_1, ex_station_2, ex_station_3, ex_station_4}\nlocal ex_subway_2 = {}\nlocal ex_subway_3 = {\n    {name=\"A\", connections={\"B\", \"D\"}},\n    {name=\"B\", connections={\"C\", \"A\"}},\n    {name=\"C\", connections={\"D\", \"B\"}},\n    {name=\"D\", connections={\"A\", \"C\"}}\n}\n\nlocal function tests()\n    lu.assertEquals(add_edge(\"Fenway\", \"Kenmore\", ex_subway_2), ex_subway_2)\n    lu.assertEquals(add_edge(\"Fenway\", \"Kenmore\", ex_subway_1), {\n        ex_station_1,\n        {name=\"Fenway\", connections={\"Newton Highlands\", \"Newton Centre\", \"Kenmore\"}}, -- Adjusted the expected order\n        ex_station_3,\n        ex_station_4\n    })\n    lu.assertEquals(add_edge(\"Kenmore\", \"Newton Centre\", ex_subway_1), ex_subway_1)\n    lu.assertEquals(add_edge(\"A\", \"C\", ex_subway_3), {\n        {name=\"A\", connections={\"B\", \"D\", \"C\"}}, -- Adjusted the expected order\n        {name=\"B\", connections={\"C\", \"A\"}},\n        {name=\"C\", connections={\"D\", \"B\"}},\n        {name=\"D\", connections={\"A\", \"C\"}}\n    })\nend\n\ntests()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "apply_to_android",
    "language": "lua",
    "prompt": "-- Phone type representation using tables\nlocal function Iphone(version)\n    return { tag = \"Iphone\", version = version }\nend\n\nlocal function Android(version)\n    return { tag = \"Android\", version = version }\nend\n\nlocal function Pixel(version)\n    return { tag = \"Pixel\", version = version }\nend\n\n-- apply_to_android: Applies a function to the androids in the list of phones\nlocal function apply_to_android(f, pList)",
    "doctests": "original",
    "original": "lua-prompts/apply_to_android.lua",
    "prompt_terminology": "verbatim",
    "tests": "local function uppercase_android(p)\n    if p.tag == \"Android\" then\n        return Android(string.upper(p.version))\n    else\n        return p\n    end\nend\n\nlocal function reverse_android(p)\n    if p.tag == \"Android\" then\n        return Android(p.version:reverse()) -- Use :reverse() method for string in Lua\n    else\n        return p\n    end\nend\n\nlocal function version_android(p)\n    if p.tag == \"Android\" then\n        return Android(p.version .. \"-v2\")\n    else\n        return p\n    end\nend\n\nlocal lu = require('luaunit')\n\nlocal function tests()\n    lu.assertEquals(apply_to_android(uppercase_android, {}), {})\n    lu.assertEquals(\n        apply_to_android(uppercase_android, {Iphone(5), Pixel(3), Android(\"Cupcake\"), Pixel(7), Android(\"Grape\"), Iphone(10), Android(\"Orange\")}),\n        {Iphone(5), Pixel(3), Android(\"CUPCAKE\"), Pixel(7), Android(\"GRAPE\"), Iphone(10), Android(\"ORANGE\")}\n    )\n    lu.assertEquals(apply_to_android(reverse_android, {Iphone(5), Pixel(3), Android(\"Cupcake\")}), {Iphone(5), Pixel(3), Android(\"ekacpuC\")})\n    lu.assertEquals(apply_to_android(version_android, {Iphone(5), Pixel(3)}), {Iphone(5), Pixel(3)})\nend\n\ntests()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "add_left",
    "language": "lua",
    "prompt": "-- Using tables to represent the NumTree\n-- A Leaf is represented as {type=\"Leaf\", value=<number>}\n-- A Node is represented as {type=\"Node\", value=<number>, left=<leftTree>, right=<rightTree>}\n\n-- add_left: NumTree -> Number\n-- Adds only the numbers on the leftmost side of the tree.\nlocal function add_left(tree)",
    "doctests": "original",
    "original": "lua-prompts/add_left.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\n\nlocal function tests()\n    lu.assertEquals(add_left({type=\"Leaf\", value=4}), 4)\n    lu.assertEquals(add_left({type=\"Node\", value=5, left={type=\"Leaf\", value=6}, right={type=\"Leaf\", value=7}}), 11)\n    lu.assertEquals(add_left({\n        type=\"Node\", value=-3, \n        left={type=\"Node\", value=3, left={type=\"Leaf\", value=0}, right={type=\"Leaf\", value=9}},\n        right={type=\"Node\", value=4, left={type=\"Leaf\", value=9}, right={type=\"Leaf\", value=10}}\n    }), 0)\nend\n\ntests()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "social_time",
    "language": "lua",
    "prompt": "-- A CallType is one of:\n-- \"zoom\"\n-- \"teams\"\n-- \"phone\"\n-- Interpretation: a type of call\nlocal CallTypes = {\n    Zoom = \"zoom\",\n    Teams = \"teams\",\n    Phone = \"phone\"\n}\n\n-- Duration attendees description\n-- Interpretation: an event in some period of time, which is either:\n-- A call using some technology, lasting some number of minutes with attendees\n-- (by name), and a description;\n-- An in-person meeting lasting some number of minutes\n-- with attendees (by name) and a description; or\n-- Time spent alone for some number of minutes with a description.\nlocal EventTypes = {\n    Call = \"Call\",\n    Mtg = \"Mtg\",\n    Alone = \"Alone\"\n}\n\n-- social-time : gets the duration of calls and meetings; 0 for alone\nlocal function social_event_time(e)\n    if e.type == EventTypes.Call then\n        return e.dur_att_desc[1]\n    elseif e.type == EventTypes.Mtg then\n        return e.dur_att_desc[1]\n    else\n        return 0\n    end\nend\n\n-- social-time : how much time was spent on calls and meetings?\nlocal function social_time(events)",
    "doctests": "original",
    "original": "lua-prompts/social_time.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\n\nlocal ex_zoom_doc = { type = EventTypes.Call, callType = CallTypes.Zoom, dur_att_desc = {22, {\"Dr. Zoidberg\"}, \"Doctor appointment\"} }\nlocal ex_teams_office = { type = EventTypes.Call, callType = CallTypes.Teams, dur_att_desc = {7, {\"Mike\", \"Tajel\"}, \"Office hours\"} }\nlocal ex_phone_spam = { type = EventTypes.Call, callType = CallTypes.Phone, dur_att_desc = {1, {\"Unknown\"}, \"Spam\"} }\n\nlocal ex_mtg_study = { type = EventTypes.Mtg, dur_att_desc = {62, {\"Rachel\", \"Ross\", \"Joey\", \"Phoebe\", \"Chandler\", \"Monica\"}, \"Study group\"} }\nlocal ex_mtg_advisor = { type = EventTypes.Mtg, dur_att_desc = {28, {\"Ali\"}, \"Research meeting\"} }\n\nlocal ex_alone_lunch = { type = EventTypes.Alone, time = 34, desc = \"lunch\" }\nlocal ex_alone_reading = { type = EventTypes.Alone, time = 25, desc = \"Reading Infinite Jest\" }\n\nlocal function assertions()\n    lu.assertEquals(social_time({}), 0)\n    lu.assertEquals(social_time({ex_zoom_doc, ex_teams_office, ex_phone_spam, ex_mtg_study, ex_mtg_advisor, ex_alone_lunch, ex_alone_reading}), 120)\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "points_and_lines",
    "language": "lua",
    "prompt": "-- A Point on a cartesian plane\nPoint = {}\nPoint.new = function(x, y)\n    local self = {}\n    self.x = x\n    self.y = y\n    return self\nend\n\n-- A Line between two points\nLine = {}\nLine.new = function(start, end_)\n    local self = {}\n    self.start = start\n    self.end_ = end_\n    return self\nend\n\n-- Computes the manhattan distance that a line covers\n-- manhattan distance = |x_1 - x_2| + |y_1 - y_2|\nfunction manhattan_distance(l)",
    "doctests": "original",
    "original": "lua-prompts/points_and_lines.lua",
    "prompt_terminology": "verbatim",
    "tests": "function assertions()\n    local lu = require('luaunit')\n    \n    lu.assertEquals(manhattan_distance(Line.new(Point.new(0, 0), Point.new(4, 3))), 7)\n    lu.assertEquals(manhattan_distance(Line.new(Point.new(1, 3), Point.new(4, 3))), 3)\n    lu.assertEquals(manhattan_distance(Line.new(Point.new(4, 2), Point.new(4, 3))), 1)\n    lu.assertEquals(manhattan_distance(Line.new(Point.new(5, 8), Point.new(4, 3))), 6)\n    lu.assertEquals(manhattan_distance(Line.new(Point.new(7, 0), Point.new(7, -5))), 5)\n    lu.assertEquals(manhattan_distance(Line.new(Point.new(4, 0), Point.new(-4, 0))), 8)\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "shapes",
    "language": "lua",
    "prompt": "-- A Shape can be a table like:\n-- Circle: {kind=\"Circle\", radius=float}\n-- Rectangle: {kind=\"Rectangle\", width=float, height=float}\n-- Triangle: {kind=\"Triangle\", side1=float, side2=float, side3=float}\n\n-- Computes the perimeter of a circle\nlocal function circle_perimeter(c)\n    return 6.28 * c.radius\nend\n\n-- Computes the perimeter of a rectangle\nlocal function rectangle_perimeter(r)\n    return 2.0 * r.width + 2.0 * r.height\nend\n\n-- Computes the perimeter of a triangle\nlocal function triangle_perimeter(t)\n    return t.side1 + t.side2 + t.side3\nend\n\n-- Computes the perimeter of any Shape\nlocal function shape_perimeter(s)",
    "doctests": "original",
    "original": "lua-prompts/shapes.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\n\nlocal function assertions()\n    lu.assertEquals(shape_perimeter({kind=\"Circle\", radius=2.0}), 12.56)\n    lu.assertEquals(shape_perimeter({kind=\"Rectangle\", width=2.0, height=4.0}), 12.0)\n    lu.assertEquals(shape_perimeter({kind=\"Triangle\", side1=3.0, side2=3.0, side3=3.0}), 9.0)\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "series",
    "language": "lua",
    "prompt": "-- A Computable is represented as a table with two fields: 'value' and 'func'\n-- It represents: Computable of (X * (X -> X))\nfunction createComputable(value, func)\n    return {\n        value = value,\n        func = func\n    }\nend\n\n-- update: Runs the computable to make the next computable in the series\nfunction update(c)",
    "doctests": "original",
    "original": "lua-prompts/series.lua",
    "prompt_terminology": "verbatim",
    "tests": "lu = require('luaunit')\nfunction add_one(x)\n    return x + 1\nend\n\nfunction repeat_string(s)\n    return s .. s\nend\n\nfunction divide_2(x)\n    return x / 2\nend\n\n-- Test Computables\nlocal zero_add_one = createComputable(5, add_one)\nlocal one_add_one = update(zero_add_one)\nlocal two_add_one = update(one_add_one)\nlocal zero_repeat_string = createComputable(\"hi\", repeat_string)\nlocal one_repeat_string = update(zero_repeat_string)\nlocal two_repeat_string = update(one_repeat_string)\nlocal zero_divide_2 = createComputable(40, divide_2)\nlocal one_divide_2 = update(zero_divide_2)\nlocal two_divide_2 = update(one_divide_2)\n\nfunction testComputables()\n    lu.assertEquals(update(zero_add_one).value, 6)\n    lu.assertEquals(update(one_add_one).value, 7)\n    lu.assertEquals(update(two_add_one).value, 8)\n    lu.assertEquals(update(zero_repeat_string).value, \"hihi\")\n    lu.assertEquals(update(one_repeat_string).value, \"hihihihi\")\n    lu.assertEquals(update(two_repeat_string).value, \"hihihihihihihihi\")\n    lu.assertEquals(update(zero_divide_2).value, 20)\n    lu.assertEquals(update(one_divide_2).value, 10)\n    lu.assertEquals(update(two_divide_2).value, 5)\nend\n\n-- Run tests\ntestComputables()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "flip_tree",
    "language": "lua",
    "prompt": "-- A NumTree is either a Leaf or a Node\n-- A Leaf can be represented as {type=\"Leaf\", value=...}\n-- A Node can be represented as {type=\"Node\", value=..., left=..., right=...}\n\n-- mirror: NumTree -> NumTree\n-- Mirrors the tree around the center point\nlocal function mirror(tree)",
    "doctests": "original",
    "original": "lua-prompts/flip_tree.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\nlocal function assertions()\n    lu.assertEquals(mirror({type=\"Leaf\", value=5}), {type=\"Leaf\", value=5})\n    lu.assertEquals(mirror({type=\"Node\", value=5, left={type=\"Leaf\", value=6}, right={type=\"Leaf\", value=7}}),\n                    {type=\"Node\", value=5, left={type=\"Leaf\", value=7}, right={type=\"Leaf\", value=6}})\n    lu.assertEquals(mirror({type=\"Node\", value=5, left={type=\"Node\", value=8, left={type=\"Leaf\", value=9}, right={type=\"Leaf\", value=10}}, right={type=\"Node\", value=4, left={type=\"Leaf\", value=3}, right={type=\"Leaf\", value=2}}}),\n                    {type=\"Node\", value=5, left={type=\"Node\", value=4, left={type=\"Leaf\", value=2}, right={type=\"Leaf\", value=3}}, right={type=\"Node\", value=8, left={type=\"Leaf\", value=10}, right={type=\"Leaf\", value=9}}})\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "collatz",
    "language": "lua",
    "prompt": "-- Collatz: Number ->  Number\n-- Counts how many steps it takes a number to converge\n-- to 1 through the collatz sequence. The collatz sequence\n-- divides by 2 if the number is even, otherwise if the number\n-- is odd, it multiplies by 3 and adds 1\n\nlocal function collatz(num)",
    "doctests": "original",
    "original": "lua-prompts/collatz.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\n\nlocal function assertions()\n    lu.assertEquals(collatz(1), 0)\n    lu.assertEquals(collatz(2), 1)\n    lu.assertEquals(collatz(4), 2)\n    lu.assertEquals(collatz(3), 7)\n    lu.assertEquals(collatz(12), 9)\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  },
  {
    "name": "hello_goodbye",
    "language": "lua",
    "prompt": "-- hello : String -> String\n-- Greets you hello!\nlocal function hello(s)\n    return \"Hello \" .. s .. \"!\"\nend\n\n-- goodbye : String -> String \n-- Greets you goodbye!\nlocal function goodbye(s)\n    return \"Goodbye \" .. s .. \"!\"\nend\n\n-- double_do_it : helper for hello_goodbye. Applies both functions to each item in the list \n-- and returns a new list with the output of both functions.\nlocal function double_do_it(f, g, l)",
    "doctests": "original",
    "original": "lua-prompts/hello_goodbye.lua",
    "prompt_terminology": "verbatim",
    "tests": "local lu = require('luaunit')\n\nlocal function assertions()\n    lu.assertEquals(hello_goodbye({}), {})\n    lu.assertEquals(hello_goodbye({\"Alice\", \"Bob\"}), {\"Hello Alice!\", \"Goodbye Alice!\", \"Hello Bob!\", \"Goodbye Bob!\"})\n    lu.assertEquals(double_do_it(goodbye, hello, {\"Alice\", \"Bob\"}), {\"Goodbye Alice!\", \"Hello Alice!\", \"Goodbye Bob!\", \"Hello Bob!\"})\n    -- Note: The commented out test in OCaml was also skipped here as Lua doesn't have native sqrt or power functions for lists of numbers.\nend\n\nassertions()\n",
    "stop_tokens": [
      "\nlocal",
      "\nfunction",
      "\n--",
      "\n\n"
    ]
  }
]