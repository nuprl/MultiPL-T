from clean_training_data import clean_lua, clean_luau, clean_racket, clean_ml

def clean_sol_prompt(lang, sol):
    if lang == "lua":
        return clean_lua(sol)
    if lang == "luau":
        return clean_luau(sol)
    elif lang == "racket":
        return clean_racket(sol)
    elif lang == "ml":
        return clean_ml(sol)
    else:
        raise Exception("Unknown language: " + lang)
