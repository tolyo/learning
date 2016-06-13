from ps3b import *

word_list = load_words()
hand = {'n': 1, 'h': 1, 'o': 1, 'y': 1, 'd':1, 'w':1, 'e': 2}

#
# Test code
#
def test_comp_choose_word():
    print "beginning test"
    print comp_choose_word(hand, word_list)
    print "finish test"

# test_comp_choose_word()
def test_comp_play_hand():
    print "Begin test"
    comp_play_hand(hand, word_list)
    print "End test"

test_comp_choose_word()

test_comp_play_hand()

play_game(word_list)