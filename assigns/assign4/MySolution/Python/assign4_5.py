def string_fset_at(cs, i0, c0):
    n = len(cs)
    result = [cs[i] if i != i0 else c0 for i in range(n)]
    return ''.join(result)

alphabet = ''.join(chr(ord('a') + i) for i in range(26))

def list_of_buddies(word):
    n0 = len(word)
    def fwork(work):
        for i0 in range(n0):
            c0 = word[i0]
            for c1 in alphabet:
                if c1 != c0:
                    work(string_fset_at(word, i0, c1))

    buddies = []
    fwork(buddies.append)
    return buddies
