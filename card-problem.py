from itertools import permutations, combinations

perms = list(permutations([0, 1, 2, 3]))
perm_index = {p : i for i, p in enumerate(perms)}

def ith_perm(i, four):
    return [four[k] for k in perms[i]]

def perm_i(four):
    s = sorted(four)
    return perm_index[tuple([s.index(x) for x in four])]

def hide_one_show_four(five, sort=True):
    if sort:
        five = sorted(five)
    y, z, *xs = five
    if z <= 24:
        return y, ith_perm(y, [z] + xs)
    elif y >= z - 24:
        return y, ith_perm(y - z + 24, [z] + xs)
    else:
        return z, ith_perm(z - 25, [y] + xs)

def guess_hidden(four):
    p, q = perm_i(four), min(four)
    if q > 24:
        return p + q - 24
    elif p >= q:
        return p + 25
    else:
        return p

for five in combinations(range(52), 5):
    # no sort necessary because combinations already sorts
    one, four = hide_one_show_four(five, sort=False)
    assert one == guess_hidden(four)
print('works!')
