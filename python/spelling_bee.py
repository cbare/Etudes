# Play and solve spelling bee puzzles

DICT_PATH = "/usr/share/dict/words"

with open(DICT_PATH, "r") as f:
    words = set(f.read().split("\n"))


def is_valid(letters, center, word):
    return (
        len(word) >= 4 
        and center in word
        and all(letter in letters for letter in word)
    )

def check_guess(letters, center, word):
    return is_valid(letters, center, word) and word in words

def score(word):
    if len(word) < 4:
        return 0, False
    elif len(word) == 4:
        return 1, False
    else:
        pangram = len(set(word)) == 7
        score = len(word) + (7 if pangram else 0)
        return score, pangram

def total_score(letters, center):
    found = []
    for word in sorted(words):
        if not word or "'" in word or word[0].isupper(): continue
        if is_valid(letters, center, word):
            x, pangram = score(word)
            found.append((word, x, pangram))
    return found

def play(letters, center):
    total_points = 0
    found = {}
    bee_words = {
        word: (s, pangram)
        for word, s, pangram in total_score(letters, center)
    }
    while True:
        print(f"Spelling Bee: ||{center}||, {", ".join(sorted(set(letters)-{center}))}")
        word = input(">> ")
        if word.lower() == 'q':
            break
        elif word.lower() == 'h':
            print(f"{len(bee_words)} words to be found for a total of {sum(s for s,_ in bee_words.values())} possible points.")
        elif word.lower() == 's':
            for word in sorted(found.keys()):
                points, pangram = found[word]
                print(f"{word:30} {points:3} {'Pangram!' if pangram else ''}")
            print(f"{len(found)} words found for a total of {total_points} points.")
        elif word in found:
            print(f"{word} already found!")
        elif len(word) < 4:
            print(f"{word} is too short!")
        elif center not in word:
            print(f"{center} missing!")
        elif not all(letter in letters for letter in word):
            print(f"Bad letters!")
        elif check_guess(letters, center, word):
            points, pangram = score(word)
            found[word] = (points, pangram)
            total_points += points
            if pangram:
                print("Pangram!! ", end="")
            print(f"{points} points! {len(found)} words found for a total of {total_points} points.")
        else:
            print("Not a word")
    print(f"{len(found)} words found for a total of {total_points} points.")

def find_all_bees(words):
    seed_words = {}
    for word in sorted(words):
        if not word or "'" in word or word[0].isupper(): continue
        word = word.lower()
        letters = set(word)
        key = "".join(sorted(letters))
        if key in seed_words: continue
        if len(letters) == 7:
            seed_words[key] = []
            for center in letters:
                found = total_score(letters, center)
                points = sum(p for _, p, _ in found)
                pangrams = sum(p for _, _, p in found)
                seed_words[key].append( (word, center, len(found), pangrams) )
                print(f"{center}-{word:30}:  {len(found):4} {points:5} {pangrams:2}")


# letters = [a for a in "ecilnmy"]
# center = "e"

letters = [a for a in "utcvied"]
center = "v"

letters = [a for a in "hilnopy"]
center = "o"


found = total_score(letters, center)
pangrams = 0
for word, x, pangram in found:
    if pangram: pangrams += 1
    print(f"{word:30}: {x:3d}")
print(f"== found {len(found)} words, {pangrams} pangrams, for {sum(p for _,p,_ in found)} points!")


