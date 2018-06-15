wordstring = "This is the essary that will be marked"

wordlist = wordstring.split()

lengths = []

for w in range(len(wordlist)):
    lengths.append(len(wordlist[w]))

average_letters_per_word = sum(lengths)/len(lengths)
number_of_words = len(lengths)
