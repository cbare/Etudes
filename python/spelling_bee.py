DICT_PATH = "/usr/share/dict/words"

with open(DICT_PATH, "r") as f:
    words = set(f.read().split("\n"))

# letters = [a for a in "ecilnmy"]
# center = "e"

letters = [a for a in "floping"]
center = "p"

def is_valid(letters, center, word):
    return (
        len(word) >= 4 
        and center in word
        and all(letter in letters for letter in word)
    )

def score(word):
    if len(word) < 4:
        return 0
    elif len(word) == 4:
        return 1
    return len(word) + (7 if len(set(word)) == 7 else 0)

found = []
for word in sorted(words):
    if is_valid(letters, center, word):
        x = score(word)
        print(f"{word:30}: {x:3d}")
        found.append((word, x))
print(f"== found {len(found)} words for {sum(p for w, p in found)} points!")
