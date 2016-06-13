from perm import *
from ps3a import *


#
#
# Problem #6A: Computer chooses a word
#
#
def comp_choose_word(hand, word_list):
    """
	Given a hand and a word_dict, find the word that gives the maximum value score, and return it.
   	This word should be calculated by considering all possible permutations of lengths 1 to HAND_SIZE.

    hand: dictionary (string -> int)
    word_list: list (string)
    """
    # Get permutaitons
    word = None
    score = None
    # For each permutation check if work is in wordlist
    for i in reversed(range(1, len(hand), 1)):
        if word != None:
            break
        perms = get_perms(hand, i)
        for perm in perms:
            if perm in word_list:
                if word is None:
                    word = perm
                    score = get_word_score(perm, len(hand))
                elif get_word_score(perm, len(hand)) > score:
                    word = perm
                    score = get_word_score(perm, len(hand))
                else:
                    continue
    return word
#
# Problem #6B: Computer plays a hand
#
def comp_play_hand(hand, word_list):
    """
     Allows the computer to play the given hand, as follows:

     * The hand is displayed.

     * The computer chooses a word using comp_choose_words(hand, word_dict).

     * After every valid word: the score for that word is displayed, 
       the remaining letters in the hand are displayed, and the computer 
       chooses another word.

     * The sum of the word scores is displayed when the hand finishes.

     * The hand finishes when the computer has exhausted its possible choices (i.e. comp_play_hand returns None).

     hand: dictionary (string -> int)
     word_list: list (string)
    """
    print "Computer begins play"
    total = 0
    while True:
        display_hand(hand)
        word = comp_choose_word(hand,word_list)
        if word == None:
            print "Computer could not find a word for a given hand"
            break
        else:
            print "Computer choose a word " + word
            hand = update_hand(hand, word)
            word_score = get_word_score(word, HAND_SIZE)
            print "The score for that word is " + str(word_score)

            total += word_score
            if hand_is_empty(hand):
                break
    print "Computer total score: " + str(total)
#
# Problem #6C: Playing a game
#
#
def play_game(word_list):
    """Allow the user to play an arbitrary number of hands.

    1) Asks the user to input 'n' or 'r' or 'e'.
    * If the user inputs 'n', play a new (random) hand.
    * If the user inputs 'r', play the last hand again.
    * If the user inputs 'e', exit the game.
    * If the user inputs anything else, ask them again.

    2) Ask the user to input a 'u' or a 'c'.
    * If the user inputs 'u', let the user play the game as before using play_hand.
    * If the user inputs 'c', let the computer play the game using comp_play_hand (created above).
    * If the user inputs anything else, ask them again.

    3) After the computer or user has played the hand, repeat from step 1

    word_list: list (string)
    """
    selection = raw_input("Please enter 'n' or 'r' or 'e' ")
    hand = None
    if selection == 'n':
        hand = deal_hand(HAND_SIZE)
        play_hand_with_comp(hand, word_list)
    elif selection == 'r':
        if hand is None:
            hand = deal_hand(HAND_SIZE)
        play_hand_with_comp(hand, word_list)
    elif selection == 'e':
        return
    else:
        play_game(word_list)


def play_hand_with_comp(hand, word_list):
    total = 0
    while True:
        selection = raw_input("Please enter 'u' or 'c'")
        if selection == 'u':
            display_hand(hand)
            word = raw_input("Please enter your word : ")
            if word == ".":
                break
            if is_word_in_hand(word, hand):
                hand = update_hand(hand, word)
                word_score = get_word_score(word, HAND_SIZE)
                print "The score for that word is " + str(word_score)

                total += word_score
                if hand_is_empty(hand):
                    break
        elif selection == 'c':
            word = comp_choose_word(hand, word_list)
            if word == None:
                print "No word chosen by computer"
                break
            print "Computer chose a word " + word
            hand = update_hand(hand, word)
            word_score = get_word_score(word, HAND_SIZE)
            print "The score for that word is " + str(word_score)

            total += word_score
            if hand_is_empty(hand):
                break
        else:
            play_hand_with_comp(hand, word_list)

    print "Your total score: " + str(total)

#
# Build data structures used for entire session and play game
#
if __name__ == '__main__':
    word_list = load_words()
    play_game(word_list)

    
