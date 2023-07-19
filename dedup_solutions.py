import numpy as np
from rouge_score import rouge_scorer


def dedup(solutions: list[str], keep_threshold=0.5):
    scorer = rouge_scorer.RougeScorer(['rougeLsum'], use_stemmer=True)
    keep_mask = np.ones(len(solutions), dtype=bool)

    for i in range(len(solutions)):
        for j in range(i+1, len(solutions)):
            if solutions[i] == solutions[j]:
                continue

            scores = scorer.score(solutions[i], solutions[j])
            rouge_score = scores['rougeLsum'].fmeasure

            if rouge_score > keep_threshold:
                keep_mask[j] = False

    deduped_solutions = np.array(solutions)[keep_mask]
    return deduped_solutions.tolist()



if __name__ == "__main__":
    SOLN_0 = """
    local function sum(list)
        local sum = 0
        for _, v in ipairs(list) do
            sum = sum + v
        end
        return sum
    end
    """
    SOLN_1 = """
    local function sum(list)
        return (function(...) 
            local _, sum = math.modf(table.unpack({...}))
            return sum
        end)(table.unpack(list))
    end
    """
    SOLN_2 = """
    local function sum(list)
        local function helper(list, index)
            if index < 1 then
                return 0
            else
                return list[index] + helper(list, index - 1)
            end
        end
        return helper(list, #list)
    end
    """

    SOLN_3 = """
    local function sum(list)
        local total = 0
        while #list > 0 do
            total = total + table.remove(list)
        end
        return total
    end
    """

    SOLN_4 = """
    local function sum(list)
        local total = 0


        for _, v in pairs(list) do
            total = total + v
        end
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
    deduped = dedup(SOLUTIONS, keep_threshold=0.5)
    for soln in deduped:
        print(soln)
        print()
