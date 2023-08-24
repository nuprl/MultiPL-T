from enum import Enum
# A CallType is one of:
# "zoom"
# "teams"
# "phone"
# Interpretation: a type of call
CallTypes = Enum("CallTypes", ["Zoom", "Teams", "Phone"])

# An Event is either a Call, Mtg, or Alone
# An Alone is a {"type": "Alone", "duration": <number>,  "description": <string>}
# A Mtg is a {"type": "Mtg", "duration": <number>,  "description": <string>, "attendees": <list>}
# A Call is a {"type": "Call", "CallType": <CallType>, "duration": <number>,  "description": <string>, "attendees": <list>}

# social-time : gets the duration of calls and meetings; 0 for alone
def social_event_time(e):
    if e["type"] == "Alone":
        return 0
    else:
        return e["duration"]

# social-time : how much time was spent on calls and meetings?
def social_time(events):
    # <solution>
    return sum(map(social_event_time, events))

# <tests>
ex_zoom_doc = { "type": "Call", "CallType": CallTypes.Zoom, "description": "Doctor appointment", "attendees": ["Dr. Zoidberg"], "duration": 22 }
ex_teams_office = { "type": "Call", "CallType": CallTypes.Teams, "description": "Office hours", "attendees": ["Mike", "Tajel"], "duration": 7}
ex_phone_spam = { "type": "Call", "CallType": CallTypes.Phone, "description": "Spam", "attendees": ["unknown"], "duration": 1 }

ex_mtg_study = { "type": "Mtg", "duration": 62, "attendees": ["Rachel", "Ross", "Joey", "Phoebe", "Chandler", "Monica"], "description": "Study group" }
ex_mtg_advisor = { "type": "Mtg", "duration": 28, "attendees": ["Ali"], "description": "Research meeting" }

ex_alone_lunch = { "type": "Alone", "duration": 34, "description": "lunch" }
ex_alone_reading = { "type": "Alone", "duration": 25, "description": "Reading Infinite Jest" }

def test():
    assert (social_time([]) == 0)
    assert (social_time([ex_zoom_doc, ex_teams_office, ex_phone_spam, ex_mtg_study, ex_mtg_advisor, ex_alone_lunch, ex_alone_reading]) == 120)

test()
