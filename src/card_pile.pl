:- module(card_pile,[
                  get_card_pile_string/2, 
                  put_card_at_end_of_pile/3,
                  get_card_pile_string_with_positions/2,
                  get_card_position_in_pile/3,
                  get_card_by_position/3,
                  get_item_by_position/3,
                  get_cards_by_suit/3,
                  get_cards_by_rank_and_suit/4,
                  remove_card_by_position/4,
                  card_list_contains_card/2,
                  card_list_contains_all_given_cards/2,
                  get_all_cards_with_higher_rank/3,
                  get_cards_with_highest_rank/2,
                  get_least_ranked_card/2,
                  remove_card/3,
                  get_cards_from_given_positions/3,
                  sort_cards_by_rank/2,
                  sort_cards_by_suit/2,
                  get_card_pile_subset_string_with_positions/3,
                  card_is_used_by_any_list_of_cards/2,
                  get_card_lists_containing_card/3
               ]).

:- use_module(cards).

/* ********************************************************************************************************
Clauses that generate string reprentation of a pile (list) of cards
******************************************************************************************************** */


/* *********************************************************************
Rule Name: get_card_pile_string/2
Purpose: Generates a string representation of a list of cards, each card separated by a space
Parameters:
   Input:
      CardPile: The pile (list) of cards to be whose string representation is to be generated
   Output: 
      CardPileString: The string representation of the card pile
Assistance Received: none
********************************************************************* */
get_card_pile_string([], "").

get_card_pile_string([FirstCard|RestCards], CardPileString) :-
   %get the string representation of all the cards in the pile except the first card
   get_card_pile_string(RestCards, IncompleteCardPileString),

   %append the string representation of the first card to the string representation of all the other cards
   card_string(FirstCard, FirstCardString),
   format(string(CardPileString), "~s ~s", [FirstCardString, IncompleteCardPileString]).

/* *********************************************************************
Rule Name: get_card_pile_string_with_positions/2
Purpose: Generates a string representation of a list of cards, each card separated by a space
            and with the position of each card in the list inside parentheses.
Parameters:
   Input:
      CardPile: The pile (list) of cards to be whose string representation is to be generated
   Output: 
      CardPileString: The string representation of the card pile
Assistance Received: none
********************************************************************* */
get_card_pile_string_with_positions(CardPile, CardPileString) :-
   get_card_pile_string_with_positions(0, CardPile, CardPileString).

get_card_pile_string_with_positions(_, [], "").

get_card_pile_string_with_positions(Position, [FirstCard|RestCards], CardPileString) :-
   NextPosition is Position + 1,
   get_card_pile_string_with_positions(NextPosition, RestCards, IncompleteCardPileString),
   card_string(FirstCard, FirstCardString),
   format(string(CardPileString), "~s(~w) ~s", [FirstCardString, Position, IncompleteCardPileString]).



/* *********************************************************************
Rule Name: get_card_pile_subset_string_with_positions/2
Purpose: Generates a string representation of a list of cards, which is a subset of another list of cards.
            Each card representation in the subset list of cards is accompanied by the position of the card 
            in the superset list of card inside parentheses
Parameters:
   Input:
      CardPile: The pile (list) of cards which is a superset of the other list of cards
      CardPileSubset: The subset of CardPile whose cards are to be displayed along with positions of the card
                        in the superset Cardpile.
   Output: 
      String: The string representation of the subset subset card pile along with positions of the cards in the superset
Assistance Received: none
********************************************************************* */
get_card_pile_subset_string_with_positions(CardPile, CardPileSubset, String) :-
   get_card_pile_subset_string_with_positions(0, CardPile, CardPileSubset, String).

get_card_pile_subset_string_with_positions(_, _, [], "").

get_card_pile_subset_string_with_positions(Position, CardPile, [FirstCard | RestCards], String) :-
   NextPosition is Position + 1,
   get_card_pile_subset_string_with_positions(NextPosition, CardPile, RestCards, IncompleteString),
   card_string(FirstCard, FirstCardString),
   get_card_position_in_pile(CardPile, FirstCard, CardPositionInCardList),
   format(string(String), "~s(~w) ~s", [FirstCardString, CardPositionInCardList, IncompleteString]).


/* ********************************************************************************************************
Clauses for accessing cards or card information from a list of cards
******************************************************************************************************** */


/* *********************************************************************
Rule Name: get_item_by_position/3
Purpose: Returns an item stored in the given position in a list
Parameters:
   Input:
      List: The list from which the item is to be accessed
      Position: The position of the item to be extracted from the list
   Output: 
      Item: The item at the given position in the list
Assistance Received: none
********************************************************************* */
get_item_by_position(_, Position, -1) :-
   Position < 0.

get_item_by_position(List, Position, -1) :-
   length(List, ListSize),
   Position >= ListSize.

get_item_by_position([FirstItem | _], 0, FirstItem).

get_item_by_position([_ | RemainingItems], Position, Item) :-
   Position > 0,
   NextPosition is Position - 1,
   get_item_by_position(RemainingItems, NextPosition, Item).


/* *********************************************************************
Rule Name: get_card_by_position/3
Purpose: Returns a card stored in the given position in a list of cards
Parameters:
   Input:
      List: The list from which the card is to be accessed
      Position: The position of the card to be accessed from the list
   Output: 
      Card: The card at the given position in the list
Assistance Received: none
********************************************************************* */
get_card_by_position(CardList, Position, Card) :-
   get_item_by_position(CardList, Position, Card).


/* *********************************************************************
Rule Name: get_cards_from_given_positions/3
Purpose: Returns a list of cards stored in the given list of positions in a list of cards
Parameters:
   Input:
      CardPile: The list for cards from which the cards are to be accessed
      Position: The list of positions of the cards to be accessed from the list of cards
   Output: 
      Card: The card at the given position in the list
Assistance Received: none
********************************************************************* */
get_cards_from_given_positions([], [], []).

get_cards_from_given_positions(_, [], []).

get_cards_from_given_positions(CardPile, [FirstPosition | RemainingPositions], Cards) :-
   %get the card in the first position in the list of positions
   get_card_by_position(CardPile, FirstPosition, Card),

   %get the list of all the cards in the positions given in the remaining list of positions
   get_cards_from_given_positions(CardPile, RemainingPositions, UnfinishedCards),

   %add the first card to the list of cards
   Cards = [Card | UnfinishedCards].



/* *********************************************************************
Rule Name: get_card_position_in_pile/3
Purpose: Returns a the position of a card stored in the given list of cards
Parameters:
   Input:
      CardPile: The list for cards from which the cards are to be accessed
      Card: The card of which the position in the card pile is to be determined
   Output: 
      Position: The position of the given card in the given card pile
Assistance Received: none
********************************************************************* */
get_card_position_in_pile(CardPile, Card, Position) :-
   get_card_position_in_pile(CardPile, Card, 0, Position).

get_card_position_in_pile([], _, _, -1).

get_card_position_in_pile([FirstCard | _], Card, CurrentPosition, ActualPosition) :-
   %if the card in the current position is the needed card, return the current position
   FirstCard = Card,
   ActualPosition = CurrentPosition.

get_card_position_in_pile([ _ | RemainingCards], Card, CurrentPosition, ActualPosition) :-
   %if the card in the current position is not the needed card, look for the card in the next position
   NextPosition is CurrentPosition + 1,
   get_card_position_in_pile(RemainingCards, Card, NextPosition, ActualPosition).



/* ********************************************************************************************************
Clauses for manipulating a list of cards
******************************************************************************************************** */

/* *********************************************************************
Rule Name: put_card_at_end_of_pile/3
Purpose: Places a given card at the end of a given list of cards
Parameters:
   Input:
      CardPile: The list for card at the end of which the given card is to be placed
      Card: The card to be placed at the end of the given list
   Output: 
      NewCardPile: The list of cards with the given card ended at the end
Assistance Received: none
********************************************************************* */

put_card_at_end_of_pile(Card, [], [Card]).
put_card_at_end_of_pile(Card, [CardAtTop | RemainingCards], NewCardPile) :-
   put_card_at_end_of_pile(Card, RemainingCards, TempCardPile),
   NewCardPile = [CardAtTop | TempCardPile].



/* *********************************************************************
Rule Name: remove_card_by_position/4
Purpose: Removes a card at the given position from the given list of cards
Parameters:
   Input:
      CardPile: The list from which a card is to be removed
      Position: The position of the list from which the card is to be removed
   Output: 
      RemainingCards: The updated list of cards from which the card at the given position has been removed
      RemovedCard: The card that was removed
Assistance Received: none
********************************************************************* */

remove_card_by_position(CardPile, Position, CardPile, -1) :-
   %if the position is out of bounds, return the CardPile unchanged and -1 as the removed card
   Position < 0.

remove_card_by_position(CardPile, Position, CardPile, -1) :-
   %if the position is out of bounds, return the CardPile unchanged and -1 as the removed card
   length(CardPile, CardPileSize),
   Position >= CardPileSize.

%if the position from which to remove the card is 0, then remove the first card
remove_card_by_position([FirstCard | RemainingCards], 0, RemainingCards, FirstCard).

remove_card_by_position([FirstCard | RemainingCards], Position, NewCardPile, RemovedCard) :-
   %if the position from which to remove the card is not 0, then see if the position is 0 in the remaining list of cards
   NextPosition is Position - 1,
   remove_card_by_position(RemainingCards, NextPosition, IntermediateCardPile, RemovedCard),
   NewCardPile = [FirstCard | IntermediateCardPile].



/* *********************************************************************
Rule Name: remove_card/3
Purpose: Removes the first occurence of a card from a given list of cards
Parameters:
   Input:
      CardPile: The list from which a card is to be removed
      Card: The card to be removed
   Output: 
      NewCardPile: The updated list of cards from which the given card has been removed
Assistance Received: none
********************************************************************* */
remove_card([], _, []).

%if the card to be removed is the first card in the list, then return the list without the first card
remove_card([Card | RemainingCards], Card, RemainingCards).

remove_card([FirstCard | RemainingCards], Card, NewCardPile) :-
   %if the card is not the first card in the list, then find and remove the card from the remaining list of cards
   remove_card(RemainingCards, Card, PileWithoutCard),
   %append the first card to the remaining list of cards from which the card to be removed has been removed
   NewCardPile = [FirstCard | PileWithoutCard].



/* ********************************************************************************************************
Clauses for accessing cards from a list of a cards based on certain conditions
******************************************************************************************************** */

/* *********************************************************************
Rule Name: get_cards_by_suit/3
Purpose: Get a list of all the cards of a given suit from a given list of cards
Parameters:
   Input:
      CardPile: The list from which the cards of the given suit are to be gotten
      Suit: The suit of which the cards from the list are to be returned
   Output: 
      CardsOfGivenSuit: The list of cards containing cards of all the given suit in the given list of cards
Assistance Received: none
********************************************************************* */
get_cards_by_suit([], _, []).

get_cards_by_suit([FirstCard | RemainingCards], Suit, CardsOfGivenSuit) :-
   %if the first card in the given list is of the given suit
   get_card_suit(FirstCard, FirstCardSuit),
   FirstCardSuit = Suit, 
   %find the list of all the cards with the given suit in the remaining list of cards
   get_cards_by_suit(RemainingCards, Suit, OtherCardsOfGivenSuit),
   %append the first card to that list of cards with the given suit from the remaining list of cards
   CardsOfGivenSuit = [FirstCard | OtherCardsOfGivenSuit].

get_cards_by_suit([FirstCard | RemainingCards], Suit, CardsOfGivenSuit) :-
   %if the first card in the given list is not of the given suit, return the list of cards of the given suit from the remaining list of cards
   get_card_suit(FirstCard, FirstCardSuit),
   FirstCardSuit \= Suit, 
   get_cards_by_suit(RemainingCards, Suit, CardsOfGivenSuit).


/* *********************************************************************
Rule Name: get_cards_by_rank_and_suit/4
Purpose: Get a list of all the cards of a given suit from a given list of cards
Parameters:
   Input:
      CardPile: The list from which the cards of the given suit are to be gotten
      Rank: The rank of which the cards from the list are to be returned
      Suit: The suit of which the cards from the list are to be returned
   Output: 
      CardsOfGivenRankAndSuit: The list of cards containing cards of all the given rank and suit in the given list of cards
Assistance Received: none
********************************************************************* */
get_cards_by_rank_and_suit([], _, _, []).

get_cards_by_rank_and_suit([FirstCard | RemainingCards], Rank, Suit, CardsOfGivenRankAndSuit) :-
   %if the first card in the given list is of the given suit and rank
   get_card_rank(FirstCard, FirstCardRank),
   FirstCardRank = Rank, 
   get_card_suit(FirstCard, FirstCardSuit),
   FirstCardSuit = Suit,
   %find the list of all the cards with the given rank and suit in the remaining list of cards
   get_cards_by_rank_and_suit(RemainingCards, Rank, Suit, OtherCardsOfGivenRankAndSuit),
   %append the first card to that list of cards with the given suit and rank from the remaining list of cards
   CardsOfGivenRankAndSuit = [FirstCard | OtherCardsOfGivenRankAndSuit].

get_cards_by_rank_and_suit([FirstCard | RemainingCards], Rank, Suit, CardsOfGivenRankAndSuit) :- 
   %if the first card in the given list is not of the given rank, return the list of cards of the given rank and suit from the remaining list of cards
   get_card_rank(FirstCard, FirstCardRank),
   FirstCardRank \= Rank,
   get_cards_by_rank_and_suit(RemainingCards, Rank, Suit, CardsOfGivenRankAndSuit).

get_cards_by_rank_and_suit([FirstCard | RemainingCards], Rank, Suit, CardsOfGivenRankAndSuit) :- 
   %if the first card in the given list is not of the given suit, return the list of cards of the given rank and suit from the remaining list of cards
   get_card_suit(FirstCard, FirstCardSuit),
   FirstCardSuit \= Suit,
   get_cards_by_rank_and_suit(RemainingCards, Rank, Suit, CardsOfGivenRankAndSuit).


/* *********************************************************************
Rule Name: get_all_cards_with_higher_rank/4
Purpose: Get a list of all the cards that have a greater rank than the given rank
Parameters:
   Input:
      CardPile: The list from which the cards are to be gotten
      Rank: The rank higher than which are the ranks of the list of cards that is to be returned
   Output: 
      CardsWithHigherRank: The list of cards with a higher rank than the given rank
Assistance Received: none
********************************************************************* */
get_all_cards_with_higher_rank([], _, []).

get_all_cards_with_higher_rank([FirstCard | RemainingCards], Rank, CardsWithHigherRank) :-
   get_card_rank(FirstCard, FirstCardRank),
   %if the card has a higher rank than the given rank
   compare_ranks(FirstCardRank, Rank, FirstCardRank),
   get_all_cards_with_higher_rank(RemainingCards, Rank, OtherCardsWithHigherRank),
   %then append the card to the list of cards with higher rank obtained from the remaining list
   CardsWithHigherRank = [FirstCard | OtherCardsWithHigherRank].

get_all_cards_with_higher_rank([FirstCard | RemainingCards], Rank, CardsWithHigherRank) :-
   get_card_rank(FirstCard, FirstCardRank),
   %if the card does not have a higher rank than the given rank
   \+ (compare_ranks(FirstCardRank, Rank, FirstCardRank)),
   %return the list of cards with higher rank obtained from the remaining list
   get_all_cards_with_higher_rank(RemainingCards, Rank, CardsWithHigherRank).



/* *********************************************************************
Rule Name: get_cards_with_highest_rank/2
Purpose: Get a list of all the cards that have the highest rank in the given list of cards
Parameters:
   Input:
      CardPile: The list from which the cards are to be gotten
   Output: 
      CardsWithHighestRank: The list of cards with the highest rank in the list
Assistance Received: none
********************************************************************* */
get_cards_with_highest_rank([], -1).

get_cards_with_highest_rank([Card], [Card]).

get_cards_with_highest_rank([FirstCard | RemainingCards], CardsWithHighestRank) :-
   get_cards_with_highest_rank(RemainingCards, [FirstCard], CardsWithHighestRank).

%if the card pile is empty, return an empty list
get_cards_with_highest_rank([], ProvisionalHighestRankedCards, ProvisionalHighestRankedCards).

get_cards_with_highest_rank([NextCardInPile | RemainingCards], [TempHighRankedCard | RemainingHighRankedCards], HighestRankCards) :-
   %if the provisional list of highest ranked cards has cards with rank greater than that of the next card in the list of cards
   card_with_higher_rank(NextCardInPile, TempHighRankedCard, TempHighRankedCard),

   %then keep current provisional list of highest ranked cards as it is and move on to compare with the next card in the list
   get_cards_with_highest_rank(RemainingCards, [TempHighRankedCard | RemainingHighRankedCards], HighestRankCards).

get_cards_with_highest_rank([NextCardInPile | RemainingCards], [TempHighRankedCard | _], HighestRankCards) :-
   %if the provisional list of highest ranked cards has cards with a smaller rank greater than that of the next card in the list of cards
   card_with_higher_rank(NextCardInPile, TempHighRankedCard, NextCardInPile),

   %then change the current provisional list of highest ranked cards to a list containing only the next card in the list
   get_cards_with_highest_rank(RemainingCards, [NextCardInPile], HighestRankCards).

get_cards_with_highest_rank([NextCardInPile | RemainingCards], [TempHighRankedCard | RemainingHighRankedCards], HighestRankCards) :-
   %if the provisional list of highest ranked cards has cards with the same rank as that of the next card in the list of cards
   card_with_higher_rank(NextCardInPile, TempHighRankedCard, same_rank),

   %then keep add the next card in the list to the provisional list of highest ranked cards
   get_cards_with_highest_rank(RemainingCards, [NextCardInPile, TempHighRankedCard | RemainingHighRankedCards], HighestRankCards).



/* *********************************************************************
Rule Name: get_least_ranked_card/2
Purpose: Get the card that has the lowest rank in the given list of cards
Parameters:
   Input:
      CardPile: The list from which the card with the lowest rank is to be gotten
   Output: 
      LeastRankedCard: The card with the lowest rank in the group of cards
Assistance Received: none
********************************************************************* */
get_least_ranked_card([], -1).

get_least_ranked_card([Card], Card).

get_least_ranked_card([FirstCard | RemainingCards], LeastRankedCard) :-
   get_least_ranked_card(RemainingCards, ProvisionalLeastRankedCard),
   card_with_same_or_less_rank(FirstCard, ProvisionalLeastRankedCard, LeastRankedCard).



/* *********************************************************************
Rule Name: get_card_lists_containing_card/3
Purpose: Get the list of all card lists that have the given card inside of them
Parameters:
   Input:
      ListOfCardLists: The list containing the list of cards
      Card: The card contained in the lists that are to be returned
   Output: 
      ListOfListsContainingCard: The list of all card lists that have the given card inside of them
Assistance Received: none
********************************************************************* */
get_card_lists_containing_card([], _, []).

get_card_lists_containing_card([FirstCardList | RemainingListOfCardLists], Card, ListOfListsContainingCard) :-
   %if the first list in the list of lists contains the needed card
   card_list_contains_card(FirstCardList, Card),
   %then fetch the list of lists that contain the needed card from the remaining list of lists of cards
   get_card_lists_containing_card(RemainingListOfCardLists, Card, IncompleteListOfLists),
   %then append the first list to that list
   ListOfListsContainingCard = [FirstCardList | IncompleteListOfLists].

get_card_lists_containing_card([FirstCardList | RemainingListOfCardLists], Card, ListOfListsContainingCard) :-
   %if the first list in the list of lists does not contain the needed card
   \+ (card_list_contains_card(FirstCardList, Card)),
   %if 
   get_card_lists_containing_card(RemainingListOfCardLists, Card, ListOfListsContainingCard).




/* ********************************************************************************************************
Clauses for that check whether a list of cards satisfy a certain condition
******************************************************************************************************** */

/* *********************************************************************
Rule Name: card_list_contains_card/2
Purpose: True only if the given card is within the given card list
Parameters:
   Input:
      CardList: The list which is to be tested for whether it contains the card
      Card: The card that is to be searched for in the card list
   Output: 
Assistance Received: none
********************************************************************* */
%if the card list is empty, the clause fails
card_list_contains_card(CardList, Card) :-
   %if the list is not empty and the first card in the list is the given card
   CardList \= [],
   [FirstCard | _] = CardList,
   FirstCard = Card.

card_list_contains_card(CardList, Card) :-
   %if the list is not empty and the first card in the list is not the given card
   CardList \= [],
   [FirstCard | RemainingCards] = CardList,
   FirstCard \= Card,
   %search for the card in the remaining list
   card_list_contains_card(RemainingCards, Card).



/* *********************************************************************
Rule Name: card_list_contains_all_given_cards/2
Purpose: True only if  all the cards in a given list of cards are within another given list of cards 
Parameters:
   Input:
      CardList: The list which is to be tested for whether it contains all the given cards
      RequiredCardsList: The list of cards that are to be searched for in the card list
   Output: 
Assistance Received: none
********************************************************************* */
card_list_contains_all_given_cards(_, []).

card_list_contains_all_given_cards(CardList, [FirstCard | RemainingCards]) :-
   %check if the list contains the first card in the RequiredCardlist
   card_list_contains_card(CardList, FirstCard),
   %if it does, do the same for the other cards in the RequiredCardlist
   card_list_contains_all_given_cards(CardList, RemainingCards).



/* *********************************************************************
Rule Name: card_is_used_by_any_list_of_cards/2
Purpose: True only if any of the lists in the given list of lists use the given card
Parameters: 
   Input:
      ListOfListsOfCards: The list of lists that have to be searched for the card
      Card: The card to be search for in the given list of lists
   Output: 
Assistance Received: none
********************************************************************* */
%if the counter reaches 0 without encountering true, then card is not used by meld; so, clause fails
card_is_used_by_any_list_of_cards(ListOfListsOfCards, Card) :-
   length(ListOfListsOfCards, NumOfListsOfCards),
   card_is_used_by_any_list_of_cards(ListOfListsOfCards, Card, NumOfListsOfCards).

card_is_used_by_any_list_of_cards([FirstListOfCards | _], Card, Counter) :-
   Counter > 0,
   card_list_contains_card(FirstListOfCards, Card).

card_is_used_by_any_list_of_cards([FirstListOfCards | RemainingListsOfCards], Card, Counter) :-
   Counter > 0, 
   \+ (card_list_contains_card(FirstListOfCards, Card)),
   NewCounter is Counter - 1,
   card_is_used_by_any_list_of_cards(RemainingListsOfCards, Card, NewCounter).



/* *********************************************************************
Rule Name: card_list_contains_card_with_given_rank_and_suit/3
Purpose: True only if the list contains a card with the given rank and suit
Parameters: 
   Input:
      CardList: The list to be checked for the given rank and suit
      Rank: The rank of the card to be searched for
      Suit: The suit of the card to be searched for
   Output: 
Assistance Received: none
********************************************************************* */

%first check if the first card that has the given rank and suit exists in the list
card_list_contains_card_with_given_rank_and_suit(CardList, Rank, Suit) :-
   get_card_id_from_rank_and_suit(Rank, Suit, CardId),
   card_list_contains_card(CardList, CardId).

%next check if the second card that has the given rank and suit exists in the list
card_list_contains_card_with_given_rank_and_suit(CardList, Rank, Suit) :-
   get_card_id_from_rank_and_suit(Rank, Suit, TempCardId),
   num_of_unique_cards_in_game(NumOfUniqueCardsInGame),
   CardId is TempCardId + NumOfUniqueCardsInGame,
   card_list_contains_card(CardList, CardId).




/* *********************************************************************
Rule Name: sort_cards_by_rank/2
Purpose: Sorts the given list of cards by the rank of the card using a bubble sort algorithm
Parameters: 
   Input:
      Cards: The unsorted list of cards
   Output: 
      SortedCards: The cards sorted by rank
Assistance Received: Prolog implementation of bubble sort taken from here: http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
********************************************************************* */

sort_cards_by_rank(Cards, SortedCards) :-
   sort_cards_by_rank(Cards, [], SortedCards).

sort_cards_by_rank([], SortedCards, SortedCards).

sort_cards_by_rank(Cards, IntermediateSortedCards, SortedCards) :-
   length(Cards, Size),
   Size \= 0,
   [FirstCard | RemainingCards] = Cards,
   bubble_cards_by_rank(FirstCard, RemainingCards, NewRemainingCards, HighestCard),
   sort_cards_by_rank(NewRemainingCards, [HighestCard | IntermediateSortedCards], SortedCards).

bubble_cards_by_rank(Card, [], [], Card).

bubble_cards_by_rank(Card, [FirstCard | RemainingCards], [FirstCard | OtherRemainingCards], HighestCard) :-
   card_with_higher_rank(Card, FirstCard, CardWithHigherRank),
   Card = CardWithHigherRank,

   bubble_cards_by_rank(Card, RemainingCards, OtherRemainingCards, HighestCard).

bubble_cards_by_rank(Card, [FirstCard | RemainingCards], [Card | OtherRemainingCards], HighestCard) :-
   card_with_same_or_less_rank(Card, FirstCard, CardWithSameOrLessRank),
   Card = CardWithSameOrLessRank,
   bubble_cards_by_rank(FirstCard, RemainingCards, OtherRemainingCards, HighestCard).


/* *********************************************************************
Rule Name: sort_cards_by_suit/2
Purpose: Sorts the given list of cards by the suits of the card using a bubble sort algorithm
Parameters: 
   Input:
      Cards: The unsorted list of cards
   Output: 
      SortedCards: The cards sorted by suit
Assistance Received: Prolog implementation of bubble sort taken from here: http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html
********************************************************************* */

sort_cards_by_suit(Cards, SortedCards) :-
   sort_cards_by_suit(Cards, [], SortedCards).

sort_cards_by_suit([], SortedCards, SortedCards).

sort_cards_by_suit(Cards, IntermediateSortedCards, SortedCards) :-
   length(Cards, Size),
   Size \= 0,
   [FirstCard | RemainingCards] = Cards,
   bubble_cards_by_suit(FirstCard, RemainingCards, NewRemainingCards, HighestCard),
   sort_cards_by_suit(NewRemainingCards, [HighestCard | IntermediateSortedCards], SortedCards).


bubble_cards_by_suit(Card, [], [], Card).

bubble_cards_by_suit(Card, [FirstCard | RemainingCards], [FirstCard | OtherRemainingCards], HighestCard) :-
   card_with_higher_suit(Card, FirstCard, CardWithHigherSuit),
   Card = CardWithHigherSuit,
   bubble_cards_by_suit(Card, RemainingCards, OtherRemainingCards, HighestCard).

bubble_cards_by_suit(Card, [FirstCard | RemainingCards], [Card | OtherRemainingCards], HighestCard) :-
   card_with_same_or_less_suit(Card, FirstCard, CardWithSameOrLessSuit),
   Card = CardWithSameOrLessSuit,
   bubble_cards_by_suit(FirstCard, RemainingCards, OtherRemainingCards, HighestCard).