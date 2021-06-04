:- module(deck, [generate_deck/1, take_card_from_top/3]).

:- use_module(cards).

/* *****************************************************************************************************
Clauses related the generation of and interaction with a deck in a round of pinochle
******************************************************************************************************* */


/* *********************************************************************
Rule Name: generate_deck/1
Purpose: Create a shuffled deck to be used in a round of pinochle
Parameters:
   Input:
   Output: 
      Deck: The generated deck
Assistance Received: none
********************************************************************* */
generate_deck(Deck) :-
   populate_deck(UnshuffledDeck),
   shuffle_deck(UnshuffledDeck, Deck).


/* *********************************************************************
Rule Name: populate_deck/1
Purpose: Create an unshuffled deck
Parameters:
   Input:
   Output: 
      CompletedDeck: The generated unshuffled deck
Assistance Received: none
********************************************************************* */
populate_deck(CompletedDeck) :-
   add_cards_to_deck(0, CompletedDeck).


/* *********************************************************************
Rule Name: add_cards_to_deck/2
Purpose: Adds card to a deck starting from the given card id
Parameters:
   Input:  
      CardId : The current CardId to be added
   Output: 
      CompletedDeck: The generated unshuffled deck containing cards starting from the given id
Assistance Received: none
********************************************************************* */
add_cards_to_deck(CardId, []) :-
   num_of_cards_in_game(NumOfCards),
   CardId >= NumOfCards.

add_cards_to_deck(CardId, CompletedDeck) :-
   NextCardId is CardId + 1,
   add_cards_to_deck(NextCardId, DeckInProgress),
   CompletedDeck = [CardId | DeckInProgress].


/* *********************************************************************
Rule Name: shuffle_deck/2
Purpose: Shuffles a deck
Parameters:
   Input:  
      UnshuffledDeck : The unshuffled deck to be shuffled
   Output: 
      ShuffledDeck: The shuffled deck
Assistance Received: none
********************************************************************* */
shuffle_deck(UnshuffledDeck, ShuffledDeck) :-
   random_permutation(UnshuffledDeck, ShuffledDeck).


/* *********************************************************************
Rule Name: take_card_from_top/3
Purpose: Removes a card from the top of a deck and returns the removed card
Parameters:
   Input:  
      CardList: The deck of which the top card is to be taken
   Output: 
      NewCardPile: The deck with the top card removed
      CardAtTop: The card removed from the top
Assistance Received: none
********************************************************************* */
take_card_from_top([], [], -1).
take_card_from_top([CardAtTop | NewCardPile], NewCardPile, CardAtTop).




