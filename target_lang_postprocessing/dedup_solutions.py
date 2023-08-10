import numpy as np
from rouge_score import rouge_scorer

def strip_comments(code: str, lang: str, strip_parens=False, trim_top_comments=True):
    comment_prefix = {
        "lua": "--",
        "python": "#",
        "javascript": "//",
        "racket": ";",
        "ml": "(*",
    }
    comment_postfix = {
        "lua": "",
        "python": "",
        "javascript": "",
        "racket": "",
        "ml": "*)",
    }
    func_start = {
        "lua": "local function",
        "python": "def",
        "javascript": "function",
        "racket": "define",
        "ml": "let",
    }

    prefix = comment_prefix.get(lang)
    postfix = comment_postfix.get(lang)
    func = func_start.get(lang)

    if not prefix:
        raise ValueError(f"Language {lang} not supported")

    func_start = code.find(func)
    top_comments = ""
    if trim_top_comments or func_start == -1:
        top_comments = code[:func_start]
        code_trim = code[func_start:]
    else:
        code_trim = code

    if postfix:
        comment_start = prefix
        comment_end = postfix
        while comment_start in code_trim and comment_end in code_trim:
            start = code_trim.find(comment_start)
            end = code_trim.find(comment_end, start + len(comment_start))
            if start != -1 and end != -1:  # Ensure both comment start and end are found
                code_trim = code_trim[:start] + code_trim[end + len(comment_end):]
            else:
                break
    else:
        # If comment postfix is empty, handle single-line comments
        lines = code_trim.split("\n")
        lines = [line for line in lines if not line.lstrip().startswith(prefix)]
        if strip_parens:
            lines = [line.replace("(", "").replace(")", "") for line in lines]
        code_trim = "\n".join(lines)

    return top_comments + code_trim


def rouge_dedup(solutions: list[str], lang="lua", dedup_threshold=0.6, trim_top_comments=True):
    scorer = rouge_scorer.RougeScorer(['rougeLsum'], use_stemmer=True)
    keep_mask = np.ones(len(solutions), dtype=bool)
    solutions_stripped = [strip_comments(sol, lang) for sol in solutions]

    for i in range(len(solutions)):
        stripped_i = solutions_stripped[i]
        for j in range(i+1, len(solutions)):
            stripped_j = solutions_stripped[j]
            if i == j or not keep_mask[j]:
                continue

            scores = scorer.score(stripped_i, stripped_j)
            rouge_score = scores['rougeLsum'].fmeasure

            if rouge_score > dedup_threshold:
                keep_mask[j] = False

    deduped_solutions = np.array(solutions)[keep_mask]
    return deduped_solutions.tolist()


if __name__ == "__main__":
    SOLN_0 = """
    -- Define a local function "sum" that takes one argument: "list".
    local function sum(list) 
        -- Initialize a local variable "sum" to 0.
        local sum = 0 
        -- For each element "v" in "list" (ignoring its index "_")
        for _, v in ipairs(list) do 
            -- Add the value of "v" to "sum"
            sum = sum + v
        end
        -- Return the result, "sum".
        return sum
    end
    """
    SOLN_1 = """
    -- Define a local function "sum" that takes one argument: "list".
    local function sum(list)
        -- Define and immediately call an anonymous function.
        return (function(...) 
            -- Unpack the elements of the list into separate arguments and call "math.modf" to separate the fractional and integral parts.
            -- The integral part is accumulated into "sum". The fractional part is ignored.
            local _, sum = math.modf(table.unpack({...}))
            -- Return the sum.
            return sum
        end)(table.unpack(list)) -- Unpack the elements of "list" as arguments to the anonymous function.
    end
    """
    SOLN_2 = """
    -- Define a local function "sum" that takes one argument: "list".
    local function sum(list)
        -- Define a local helper function for recursive sum.
        local function helper(list, index)
            -- If the index is less than 1, return 0. This is the base case for the recursion.
            if index < 1 then
                return 0
            else
                -- Otherwise, add the current index's value to the sum of the rest of the list.
                return list[index] + helper(list, index - 1)
            end
        end
        -- Call the helper function, starting from the end of the list.
        return helper(list, #list)
    end
    """

    SOLN_3 = """
    -- Define a local function "sum" that takes one argument: "list".
    local function sum(list)
        -- Initialize a local variable "total" to 0.
        local total = 0
        -- Continue the loop as long as the list has elements.
        while #list > 0 do
            -- Remove the last element from "list" and add it to "total".
            total = total + table.remove(list)
        end
        -- Return the total.
        return total
    end
    """

    SOLN_4 = """
    -- Define a local function "sum" that takes one argument: "list".
    local function sum(list)
        -- Initialize a local variable "total" to 0.
        local total = 0
        -- For each element "v" in "list" (ignoring its index "_")
        for _, v in pairs(list) do
            -- Add the value of "v" to "total"
            total = total + v
        end
        -- Return the total.
        return total
    end
    """

    SOLUTIONS = [
        SOLN_0,
        SOLN_1,
        SOLN_2,
        SOLN_3,
        SOLN_4,
    ]
    deduped = rouge_dedup(SOLUTIONS, dedup_threshold=0.6)
    for soln in deduped:
        print(soln)
        print()
