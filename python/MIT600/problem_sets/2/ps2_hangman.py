# 6.00 Problem Set 3
# 
# Hangman
#


# -----------------------------------
# Helper code
# (you don't need to understand this helper code)
import random
import string

WORDLIST_FILENAME = "words.txt"

def load_words():
    """
    Returns a list of valid words. Words are strings of lowercase letters.
    
    Depending on the size of the word list, this function may
    take a while to finish.
    """
    print "Loading word list from file..."
    # inFile: file
    inFile = open(WORDLIST_FILENAME, 'r', 0)
    # line: string
    line = inFile.readline()
    # wordlist: list of strings
    wordlist = string.split(line)
    print "  ", len(wordlist), "words loaded."
    return wordlist

def choose_word(wordlist):
    """
    wordlist (list): list of words (strings)

    Returns a word from wordlist at random
    """
    return random.choice(wordlist)

# end of helper code
# -----------------------------------

# actually load the dictionary of words and point to it with
# the wordlist variable so that it can be accessed from anywhere
# in the program
wordlist = load_words()

# your code begins here!
word = choose_word(wordlist)
num_guess = 8
print "Welcome to the game, Hangman! \nI am thinking of a word that is " + \
        str(len(word)) + \
        " letters long."

alphabet = list('abcdefghijklmnopqrstuvwxyz')
obscured = []
for i in word:
    obscured.append("_")

while True:
    print "------------"
    print "You have " + str(num_guess) + " guesses left."
    print "Available letters " + "".join(alphabet)
    guessed_letter = raw_input("Please guess a letter: ").lower()

    if guessed_letter not in alphabet:
        print "You already guessed letter : " + guessed_letter
    elif guessed_letter in word:
        alphabet.remove(guessed_letter)
        for i in range(0, len(word)):
            if word[i] == guessed_letter:
                obscured[i] = guessed_letter
        print "Good guess: " + "".join(obscured)
    else:
        alphabet.remove(guessed_letter)
        num_guess -= 1
        print "Oops! That letter is not in my word: " + "".join(obscured)

    if num_guess == 0:
        print "You lost"
        break
    if "_" not in obscured:
        print "You won"
        break