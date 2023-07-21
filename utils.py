

def clean_sol_prompt(lang, sol):
    # TODO: do this for other langs that are not lua
    assert lang == "lua", "Gotta implement this for other langs"
    sol = sol[:sol.find("\nlu =")]
    sol_lines = sol.split("\n")
    if "** Canonical Python Solution **" in sol_lines[0]:
        # remove the canonical solution from comment
        # canonical solution lines start with " * "
        not_canonical_i = 0
        for i, line in enumerate(sol_lines):
            if not line.startswith("-- *"):
                not_canonical_i = i
                break
        sol_lines = sol_lines[not_canonical_i:]

    # remove every line that is empty
    sol_lines = [line for line in sol_lines if line.rstrip() !=
                 "--" and line != ""]

    sol = "\n".join(sol_lines)

    return sol
