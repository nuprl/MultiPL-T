from clean_training_data import clean_lua, clean_luau, clean_py, clean_r, clean_racket, clean_ml, clean_julia


def clean_sol_prompt(lang, sol):
    if lang == "lua":
        return clean_lua(sol)
    elif lang == "luau":
        return clean_luau(sol)
    elif lang == "py":
        return clean_py(sol)
    elif lang == "racket":
        return clean_racket(sol)
    elif lang == "ml":
        return clean_ml(sol)
    elif lang == "r":
        return clean_r(sol)
    elif lang == "julia":
        return clean_julia(sol)
    else:
        raise Exception("Unknown language: " + lang)
