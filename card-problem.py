# https://math.stackexchange.com/a/4935029/185635
from itertools import permutations, combinations

perms = list(permutations([0, 1, 2, 3]))
perm_index = {p : i for i, p in enumerate(perms)}

def ith_perm(i, four): return [four[k] for k in perms[i]]

def i_of_perm(four):
    s = sorted(four)
    return perm_index[tuple(s.index(x) for x in four)]

def hide_one_show_four(five):
    y, z, *xs = five  # assumes sorted
    if z <= 24: return y, ith_perm(y, [z] + xs)
    elif y >= z - 24: return y, ith_perm(y - z + 24, [z] + xs)
    else: return z, ith_perm(z - 25, [y] + xs)

def guess_hidden(four):
    i, q = i_of_perm(four), min(four)
    if q > 24: return i + q - 24
    elif i >= q: return i + 25
    else: return i

for five in combinations(range(52), 5):
    one, four = hide_one_show_four(five)
    assert one == guess_hidden(four)
print('works for all inputs!')
