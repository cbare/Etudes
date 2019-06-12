import random


def pivot(items, a, b):
    p = items[b]
    i = a
    for j in range(a,b):
        if items[j] <= p:
            items[i], items[j] = items[j], items[i]
            i += 1
    items[i], items[b] = items[b], items[i]
    return i


def quicksort(items, i, j):
    """
    inplace quicksort
    """
    if i < j:
        p = pivot(items, i, j)
        quicksort(items, i, p-1)
        quicksort(items, p+1, j)


letters = random.choices('abcdefghijklmnopqrstuvwxyz', k=100)
quicksort(letters, 0, len(letters)-1)
print(''.join(letter for letter in letters))
