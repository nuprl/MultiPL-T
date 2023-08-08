from clean_training_data import clean_lua, clean_racket, clean_ml

def clean_sol_prompt(lang, sol):
    if lang == "lua":
        return clean_lua(sol)
    elif lang == "racket":
        return clean_racket(sol)
    elif lang == "ml":
        return clean_ml(sol)
    else:
        raise Exception("Unknown language: " + lang)
