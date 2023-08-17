-- A CallType is one of:
-- "zoom"
-- "teams"
-- "phone"
-- Interpretation: a type of call
local CallTypes = {
    Zoom = "zoom",
    Teams = "teams",
    Phone = "phone"
}

-- Duration attendees description
-- Interpretation: an event in some period of time, which is either:
-- A call using some technology, lasting some number of minutes with attendees
-- (by name), and a description;
-- An in-person meeting lasting some number of minutes
-- with attendees (by name) and a description; or
-- Time spent alone for some number of minutes with a description.
local EventTypes = {
    Call = "Call",
    Mtg = "Mtg",
    Alone = "Alone"
}

-- social-time : gets the duration of calls and meetings; 0 for alone
local function social_event_time(e)
    if e.type == EventTypes.Call then
        return e.dur_att_desc[1]
    elseif e.type == EventTypes.Mtg then
        return e.dur_att_desc[1]
    else
        return 0
    end
end

-- social-time : how much time was spent on calls and meetings?
local function social_time(events)
    -- <solution>
    local total = 0
    for _, event in ipairs(events) do
        total = total + social_event_time(event)
    end
    return total
end

-- <tests>
local lu = require('luaunit')

local ex_zoom_doc = { type = EventTypes.Call, callType = CallTypes.Zoom, dur_att_desc = {22, {"Dr. Zoidberg"}, "Doctor appointment"} }
local ex_teams_office = { type = EventTypes.Call, callType = CallTypes.Teams, dur_att_desc = {7, {"Mike", "Tajel"}, "Office hours"} }
local ex_phone_spam = { type = EventTypes.Call, callType = CallTypes.Phone, dur_att_desc = {1, {"Unknown"}, "Spam"} }

local ex_mtg_study = { type = EventTypes.Mtg, dur_att_desc = {62, {"Rachel", "Ross", "Joey", "Phoebe", "Chandler", "Monica"}, "Study group"} }
local ex_mtg_advisor = { type = EventTypes.Mtg, dur_att_desc = {28, {"Ali"}, "Research meeting"} }

local ex_alone_lunch = { type = EventTypes.Alone, time = 34, desc = "lunch" }
local ex_alone_reading = { type = EventTypes.Alone, time = 25, desc = "Reading Infinite Jest" }

local function assertions()
    lu.assertEquals(social_time({}), 0)
    lu.assertEquals(social_time({ex_zoom_doc, ex_teams_office, ex_phone_spam, ex_mtg_study, ex_mtg_advisor, ex_alone_lunch, ex_alone_reading}), 120)
end

assertions()
