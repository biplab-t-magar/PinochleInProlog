:- module(melds, [
                  get_melds_string/3,
                  meld_index/2,
                  meld_points/2,
                  meld_string/2,
                  get_all_melds_in_hand/4,
                  meld_worth_more_points/3,
                  compare_hands_for_melds/5,
                  total_number_of_melds_in_hand/4,
                  add_meld_to_storage/4,
                  create_meld/3,
                  is_a_playable_meld/3,
                  generate_empty_melds_storage/1,
                  generate_list_containing_empty_lists/2,
                  get_all_melds_containing_card/3
                  ]).

:- use_module(card_pile).
:- use_module(cards).


/* ********************************************************************************************************
Clauses that denote the definition of melds in the game
******************************************************************************************************** */
/* 
   The valid melds in a game of Pinochle are flush, royal_marriage, marriage, dix, four_aces, four_kings, four_queens, four_jacks, pinochle
*/


/*
   The number of melds defined in the game of Pinochle
*/
num_of_melds_in_game(9).

/*
   The index of each meld
*/
meld_index(flush, 0).
meld_index(royal_marriage, 1).
meld_index(marriage, 2).
meld_index(dix, 3).
meld_index(four_aces, 4).
meld_index(four_kings, 5).
meld_index(four_queens, 6).
meld_index(four_jacks, 7).
meld_index(pinochle, 8).

/*
   The string representations of each meld
*/
meld_string(flush, "Flush").
meld_string(royal_marriage, "Royal Marriage").
meld_string(marriage, "Marriage").
meld_string(dix, "Dix").
meld_string(four_aces, "Four Aces").
meld_string(four_kings, "Four Kings").
meld_string(four_queens, "Four Queens").
meld_string(four_jacks, "Four Jacks").
meld_string(pinochle, "Pinochle").


/*
   The points that each meld is worth
*/
meld_points(flush, 150).
meld_points(royal_marriage, 40).
meld_points(marriage, 20).
meld_points(dix, 10).
meld_points(four_aces, 100).
meld_points(four_kings, 80).
meld_points(four_queens, 60).
meld_points(four_jacks, 40).
meld_points(pinochle, 40).



/* *********************************************************************
Rule Name: convert_melds_to_points/3
Purpose: Converts a list containg the counts of each meld into a list of all points obtainable from that list of meld counts
Parameters:
   Input:
      CountsOfEachMeld: list containg the counts of each meld
   Output: 
      Points: The list of all points obtainable from that list of meld counts
Assistance Received: none
********************************************************************* */
convert_melds_to_points(CountsOfEachMeld, Points) :-
   convert_melds_to_points(CountsOfEachMeld, 0, Points).

convert_melds_to_points([], _, []).

convert_melds_to_points([FirstMeldCount | RemainingMeldCounts], MeldIndex, PointsList) :-
   meld_index(Meld, MeldIndex),
   meld_points(Meld, MeldPoints),

   %Create as many copies of points corresponding to a meld as there are instances of that meld
   make_list_of_copies(MeldPoints, FirstMeldCount, MeldPointsList),
   NewMeldIndex is MeldIndex + 1,
   convert_melds_to_points(RemainingMeldCounts, NewMeldIndex, IncompletePointsList),
   append(MeldPointsList, IncompletePointsList, PointsList).



/* *********************************************************************
Rule Name: make_list_of_copies/3
Purpose: Creates a list containing copies of a given value
Parameters:
   Input:
      ValueToCopy: the value to be made copies of
      NumOfCopies: the number of copies of the value to be made
   Output: 
      List: The list of all the copies of the given value
Assistance Received: none
********************************************************************* */
make_list_of_copies(_, NumberOfCopies, []) :-
   NumberOfCopies =< 0.

make_list_of_copies(ValueToCopy, NumberOfCopies, List) :-
   NewNumberOfCopies is NumberOfCopies - 1,
   make_list_of_copies(ValueToCopy, NewNumberOfCopies, IncompleteList),
   List = [ValueToCopy | IncompleteList].




/* *********************************************************************
Rule Name: meld_worth_more_points/3
Purpose: Get the meld that is worth more points
Parameters:
   Input:
      Meld1: One of the melds to be compared
      Meld2: The other meld to be compared
   Output: 
      WinningMeld: The meld that has is worth more points
Assistance Received: none
********************************************************************* */
meld_worth_more_points(Meld1, Meld2, Meld1) :-
   meld_points(Meld1, Meld1Points),
   meld_points(Meld2, Meld2Points),
   Meld1Points > Meld2Points.

meld_worth_more_points(Meld1, Meld2, Meld2) :-
   meld_points(Meld1, Meld1Points),
   meld_points(Meld2, Meld2Points),
   Meld1Points < Meld2Points.

meld_worth_more_points(Meld1, Meld2, draw) :-
   meld_points(Meld1, Meld1Points),
   meld_points(Meld2, Meld2Points),
   Meld1Points = Meld2Points.


/* ********************************************************************************************************
Clauses that get the string representation of a meld
******************************************************************************************************** */

/* *********************************************************************
Rule Name: get_melds_string/3
Purpose: Get the string representation of the melds in the given list of melds
Parameters:
   Input:
      Melds: A list of melds
      Hand: The hand of the player who played the melds
   Output: 
      MeldsString: The string representation of the melds
Assistance Received: none
********************************************************************* */
get_melds_string(Melds, Hand, MeldsString) :-
   get_melds_string(0, Melds, Hand, MeldsString).

get_melds_string(_, [], _, "").

get_melds_string(MeldIndex, _, _, "") :-
   num_of_melds_in_game(NumOfMelds),
   MeldIndex >= NumOfMelds.

get_melds_string(MeldIndex, [MeldsOfFirstMeldType | RemainingMelds], Hand, MeldsString) :-
   NextMeldIndex is MeldIndex + 1,
   meld_index(MeldType, MeldIndex),
   meld_string(MeldType, MeldTypeString),
   get_melds_string(NextMeldIndex, RemainingMelds, Hand, MeldsStringInProgress),
   get_melds_string_of_meld_type(MeldTypeString, MeldsOfFirstMeldType, Hand, MeldsStringOfFirstMeldType),
   format(string(MeldsString), "~s~s", [MeldsStringOfFirstMeldType, MeldsStringInProgress]).


/* *********************************************************************
Rule Name: get_melds_string_of_meld_type/3
Purpose: Get the string representation of the melds in a list of melds of only a given meld type
Parameters:
   Input:
      MeldTypeString: A string representation of a meld type
      Melds: The list of melds of only a given type
   Output: 
      MeldsStringOfGivenMeldType: The string representation of the melds
Assistance Received: none
********************************************************************* */
get_melds_string_of_meld_type(_, [], _, "").

get_melds_string_of_meld_type(MeldTypeString, [FirstMeld | RemainingMelds], Hand, MeldsStringOfGivenMeldType) :-
   meld_cards_into_string(FirstMeld, Hand, MeldCardsString),
   get_melds_string_of_meld_type(MeldTypeString, RemainingMelds, Hand, MeldStringInProgress),
   format(string(MeldsStringOfGivenMeldType), "~s[~s], ~s", [MeldCardsString, MeldTypeString, MeldStringInProgress]).



/* *********************************************************************
Rule Name: meld_cards_into_string/3
Purpose: Get the string representation of a list of cards representing a meld
Parameters:
   Input:
      CardList: A list of cards representing a meld
      Hand: The player's hand
   Output: 
      MeldCardString: The string representation of the meld
Assistance Received: none
********************************************************************* */
meld_cards_into_string([], _, "").

meld_cards_into_string([FirstCard | RemainingCards], Hand, MeldCardsString) :-
   card_string(FirstCard, FirstCardString),
   get_card_position_in_pile(Hand, FirstCard, Position),
   position_to_string(Position, PositionString),
   meld_cards_into_string(RemainingCards, Hand, MeldCardsStringInProgress),
   format(string(MeldCardsString), "~s(~s) ~s", [FirstCardString, PositionString, MeldCardsStringInProgress]).


position_to_string(-1, "").
position_to_string(Position, PositionString) :-
   number_string(Position, PositionString).




/* ********************************************************************************************************
Clauses that to add melds to meld storage
******************************************************************************************************** */


/* *********************************************************************
Rule Name: add_meld_to_storage/4
Purpose: Add a meld to the list structure that stores player melds
Parameters:
   Input:
      MeldsStorage: The list structure that stores melds 
      MeldCards: The cards comprising the meld that needs to be stored
      Meld: The type of the meld to be stored
   Output: 
      UpdatedMeldsStorage: The updated list structure that stores melds with the new meld included
Assistance Received: none
********************************************************************* */
add_meld_to_storage(MeldsStorage, MeldCards, Meld, UpdatedMeldsStorage) :-
   add_meld_to_storage(0, MeldsStorage, MeldCards, Meld, UpdatedMeldsStorage).

add_meld_to_storage(CurrentPosition, _, _, _, []) :-
   num_of_melds_in_game(NumOfMelds),
   CurrentPosition >= NumOfMelds.

add_meld_to_storage(CurrentPosition, [FirstMeldsList | RemainingMeldsList], MeldCards, Meld, UpdatedMeldsStorage) :-
   %if the current index in the melds storage corresponds to the index of the meld needed to be added, then add the meld to this index of the melds storage
   meld_index(Meld, MeldIndex),
   CurrentPosition = MeldIndex,
   UpdatedFirstMeldsList = [MeldCards | FirstMeldsList],
   UpdatedMeldsStorage = [UpdatedFirstMeldsList | RemainingMeldsList].
   

add_meld_to_storage(CurrentPosition, [FirstMeldsList | RemainingMeldsList], MeldCards, Meld, UpdatedMeldsStorage) :-
   NewCurrentPosition is CurrentPosition + 1,
   add_meld_to_storage(NewCurrentPosition, RemainingMeldsList, MeldCards, Meld, IncompleteMeldsStorage),
   UpdatedMeldsStorage = [FirstMeldsList | IncompleteMeldsStorage].





/* ********************************************************************************************************
Clauses that to evaluate conditions on melds
******************************************************************************************************** */


/* *********************************************************************
Rule Name: card_is_used_by_meld/3
Purpose: True if a card has been used to create the given meld
Parameters:
   Input:
      MeldsStorage: The melds played by a player
      Card: The card to be searched for in the lists of melds of given type played
      Meld: The type of the meld to be searched for
   Output: 
Assistance Received: none
********************************************************************* */
card_is_used_by_meld(MeldsStorage, Card, Meld) :-
   meld_index(Meld, MeldIndex),
   get_item_by_position(MeldsStorage, MeldIndex, MeldsOfGivenType),
   card_is_used_by_any_list_of_cards(MeldsOfGivenType, Card).


/* *********************************************************************
Rule Name: card_is_used_by_any_meld/2
Purpose: True if a card has been used to create any meld
Parameters:
   Input:
      MeldsPlayed: The melds played by a player
      Card: The card to be searched for in the list of lists of melds
   Output: 
Assistance Received: none
********************************************************************* */
card_is_used_by_any_meld(MeldsPlayed, Card) :-
   num_of_melds_in_game(NumOfMelds),
   card_is_used_by_any_meld(MeldsPlayed, Card, NumOfMelds).

card_is_used_by_any_meld([FirstMeld | _], Card, Counter) :-
   Counter > 0,
   card_is_used_by_any_list_of_cards(FirstMeld, Card).

card_is_used_by_any_meld([FirstMeld | RemainingMeldTypes], Card, Counter) :- 
   Counter > 0, 
   \+ (card_is_used_by_any_list_of_cards(FirstMeld, Card)),
   NewCounter is Counter - 1,
   card_is_used_by_any_meld(RemainingMeldTypes, Card, NewCounter).

/* *********************************************************************
Rule Name: cards_are_used_for_same_meld/2
Purpose: True if all the cards in the list have collectively been used to create the same meld
Parameters:
   Input:
      MeldsPlayed: The melds played by a player
      CardsList: The list of cards to be checked for if they are all used to create the same meld
   Output: 
Assistance Received: none
********************************************************************* */

cards_are_used_for_same_meld(MeldsPlayed, CardsList) :-
   MeldsPlayed \= [],
   CardsList \= [],
   [MeldsOfFirstMeldType | _] = MeldsPlayed,
   cards_are_used_by_meld_of_given_type(MeldsOfFirstMeldType, CardsList).

cards_are_used_for_same_meld(MeldsPlayed, CardsList) :-
   MeldsPlayed \= [],
   CardsList \= [],
   [MeldsOfFirstMeldType | MeldsOfRemainingMeldTypes] = MeldsPlayed,
   \+ (cards_are_used_by_meld_of_given_type(MeldsOfFirstMeldType, CardsList)),
   cards_are_used_for_same_meld(MeldsOfRemainingMeldTypes, CardsList).


/* *********************************************************************
Rule Name: cards_are_used_by_meld_of_given_type/2
Purpose: True if all the cards in the list have collectively been used to create a meld of the given type
Parameters:
   Input:
      MeldsOfGivenType: The list of melds of a given type
      CardsList: The list of cards to be checked for if they are all used to create the melds of the given type
   Output: 
Assistance Received: none
********************************************************************* */
cards_are_used_by_meld_of_given_type(MeldsOfGivenType, CardsList) :-
   MeldsOfGivenType \= [],
   CardsList \= [],
   [FirstMeldOfGivenType | _] = MeldsOfGivenType,
   card_list_contains_all_given_cards(FirstMeldOfGivenType, CardsList).

cards_are_used_by_meld_of_given_type(MeldsOfGivenType, CardsList) :-
   MeldsOfGivenType \= [],
   CardsList \= [],
   [FirstMeldOfGivenType | RemainingMeldsOfGivenType] = MeldsOfGivenType,
   \+ (card_list_contains_all_given_cards(FirstMeldOfGivenType, CardsList)),
   cards_are_used_by_meld_of_given_type(RemainingMeldsOfGivenType, CardsList).



/* ********************************************************************************************************
Clauses that for gettings melds 
******************************************************************************************************* */


/* *********************************************************************
Rule Name: get_all_melds_in_hand/4
Purpose: Returns all the possible melds in a player's hand
Parameters:
   Input:
      Hand: The hand of the player
      MeldsPlayed: The melds already played by the player
      TrumpSuit: The trump suit for the round
   Output: 
      AllMeldsInHand: All the possiblem melds in the player's and
Assistance Received: none
********************************************************************* */
get_all_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, AllMeldsInHand) :-
   get_all_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, 0, AllMeldsInHand).

get_all_melds_in_hand(_, _, _, StartingMeldIndex, []) :-
   num_of_melds_in_game(NumOfMelds),
   StartingMeldIndex >= NumOfMelds.

get_all_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, StartingMeldIndex, AllMeldsInHand) :-
   meld_index(MeldType, StartingMeldIndex),
   get_melds_from_hand(Hand, MeldsPlayed, MeldType, TrumpSuit, MeldsOfGivenType),
   NextIndex is StartingMeldIndex + 1,
   get_all_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, NextIndex, MeldsOfOtherTypes),
   AllMeldsInHand = [MeldsOfGivenType | MeldsOfOtherTypes].



/* *********************************************************************
Rule Name: get_all_melds_containing_card/4
Purpose: Returns all the melds containing the given card
Parameters:
   Input:
      MeldsStorage: The storage of all th melds played by a player
      Card: The card which is to be searched in all the melds
   Output: 
      AllMeldsThatContainCard: List of all the melds containing the given card
Assistance Received: none
********************************************************************* */
get_all_melds_containing_card([], _, []).

get_all_melds_containing_card([FirstMeldTypeMelds | RemainingMeldTypes], Card, AllMeldsThatContainCard) :-
   get_card_lists_containing_card(FirstMeldTypeMelds, Card, MeldsContainingCard),
   get_all_melds_containing_card(RemainingMeldTypes, Card, OtherMeldsContainingCard),
   append(MeldsContainingCard, OtherMeldsContainingCard, AllMeldsThatContainCard).





/* *********************************************************************
Rule Name: get_melds_from_hand/5
Purpose: Returns all the melds of the given type from the player's hand
Parameters:
   Input:
      Hand: The player's hand
      MeldsPlayed: The storage of all th melds played by a player
      Meld: The type of the meld whose instances are to be returned
      Trump suit: The trump suit for the round
   Output: 
      MeldsInHand: List of all the melds of given type in the player's hand
Assistance Received: none
********************************************************************* */
get_melds_from_hand(Hand, MeldsPlayed, flush, TrumpSuit, FlushesInHand) :-
   rank_index(ace, Index),
   get_same_suit_melds(flush, Hand, MeldsPlayed, TrumpSuit, Index, 5, FlushesInHand).

get_melds_from_hand(Hand, MeldsPlayed, royal_marriage, TrumpSuit, RoyalMarriagesInHand) :-
   rank_index(king, Index),
   get_same_suit_melds(royal_marriage, Hand, MeldsPlayed, TrumpSuit, Index, 2, RoyalMarriagesInHand).

get_melds_from_hand(Hand, MeldsPlayed, marriage, TrumpSuit, MarriagesInHand) :-
   get_marriages(Hand, MeldsPlayed, TrumpSuit, MarriagesInHand).

get_melds_from_hand(Hand, MeldsPlayed, dix, TrumpSuit, DixesInHand) :-
   get_dixes(Hand, MeldsPlayed, TrumpSuit, DixesInHand).

get_melds_from_hand(Hand, MeldsPlayed, four_aces, _, FourAcesInHand) :-
   get_same_rank_melds(four_aces, Hand, MeldsPlayed, ace, FourAcesInHand).

get_melds_from_hand(Hand, MeldsPlayed, four_kings, _, FourKingsInHand) :-
   get_same_rank_melds(four_kings, Hand, MeldsPlayed, king, FourKingsInHand).

get_melds_from_hand(Hand, MeldsPlayed, four_queens, _, FourQueensInHand) :-
   get_same_rank_melds(four_queens, Hand, MeldsPlayed, queen, FourQueensInHand).

get_melds_from_hand(Hand, MeldsPlayed, four_jacks, _, FourJacksInHand) :-
   get_same_rank_melds(four_jacks, Hand, MeldsPlayed, jack, FourJacksInHand).

get_melds_from_hand(Hand, MeldsPlayed, pinochle, _, PinochlesInHand) :-
   get_pinochles(Hand, MeldsPlayed, PinochlesInHand).



/* *********************************************************************
Rule Name: get_marriages/4
Purpose: Returns all the marriages from the player's hand
Parameters:
   Input:
      Hand: The player's hand
      MeldsPlayed: The storage of all the melds played by a player
      Trump suit: The trump suit for the round
   Output: 
      MarriagesInHand: List of all the marriages of given type in the player's hand
Assistance Received: none
********************************************************************* */
get_marriages(Hand, MeldsPlayed, TrumpSuit, MarriagesInHand) :-
   get_marriages(Hand, MeldsPlayed, TrumpSuit, 0, MarriagesInHand).


get_marriages(_, _, _, SuitIndex, []) :-
   num_of_suits_in_game(NumOfSuits),
   SuitIndex >= NumOfSuits.

get_marriages(Hand, MeldsPlayed, TrumpSuit, SuitIndex, MarriagesInHand) :-
   suit_index(Suit, SuitIndex),
   Suit = TrumpSuit,
   NextSuitIndex is SuitIndex + 1,
   get_marriages(Hand, MeldsPlayed, TrumpSuit, NextSuitIndex, MarriagesInHand).

get_marriages(Hand, MeldsPlayed, TrumpSuit, SuitIndex, MarriagesInHand) :-
   suit_index(Suit, SuitIndex),
   Suit \= TrumpSuit,
   rank_index(king, RankIndex),
   get_same_suit_melds(marriage, Hand, MeldsPlayed, Suit, RankIndex, 2, SomeMarriagesInHand),
   NextSuitIndex is SuitIndex + 1,
   get_marriages(Hand, MeldsPlayed, TrumpSuit, NextSuitIndex, OtherMarriagesInHand),
   append(SomeMarriagesInHand, OtherMarriagesInHand, MarriagesInHand).


/* *********************************************************************
Rule Name: get_dixes/4
Purpose: Returns all the dixes from the player's hand
Parameters:
   Input:
      Hand: The player's hand
      MeldsPlayed: The storage of all the melds played by a player
      Trump suit: The trump suit for the round
   Output: 
      MarriagesInHand: List of all the dixes of given type in the player's hand
Assistance Received: none
********************************************************************* */
get_dixes(Hand, MeldsPlayed, TrumpSuit, DixesInHand) :-
   get_cards_by_rank_and_suit(Hand, nine, TrumpSuit, UnfilteredNineOfTrumps),
   filter_out_used_cards(dix, MeldsPlayed, UnfilteredNineOfTrumps, FilteredNineOfTrumps),
   create_melds_from_eligible_cards([FilteredNineOfTrumps], DixesInHand).

get_pinochles(Hand, MeldsPlayed, PinochlesInHand) :-
   %get all Jack of Diamonds in hand
   get_cards_by_rank_and_suit(Hand, jack, diamonds, UnfilteredJDs),
   %get all Queen of Spades in hand
   get_cards_by_rank_and_suit(Hand, queen, spades, UnfilteredQSs),
   %get rid of all JDs and QSs already used to create a Pinochle
   filter_out_used_cards(pinochle, MeldsPlayed, UnfilteredJDs, AllJackOfDiamonds),
   filter_out_used_cards(pinochle, MeldsPlayed, UnfilteredQSs, AllQueenOfSpades),
   %create melds
   create_melds_from_eligible_cards([AllJackOfDiamonds, AllQueenOfSpades], PinochlesInHand).



/* *********************************************************************
Rule Name: get_same_suit_melds/7
Purpose: gets each possible SameSuit melds of the given suit from the given hand.
            Same Suit melds are those melds that have cards with the same suit but of different ranks,
            for example: Flush, Royal Marriage, and Marriage
Parameters:
   Input:
      Meld: the meld type whose instances are to be returned
      Hand: the hand of the player 
      MeldsPlayed: the melds played by the player
      Suit: the suit of the Same Suit meld
      StartingRankIndex: the highest rank comprising the meld
      NumOfCardsInMeld: the number of cards in the meld
   Output: 
      SameSuitMelds: List of all the same suit melds of given type in the player's hand
   Algorithm:
         1) loop through all the cards in hand to get the cards of each rank with the required suit needed for the meld
         2) If a card has not already been used to create the given meld, store it
         3) If the meld type concerned is a royal marriage, each King-Queen pairing has been used together to create a meld before
         4)    If no, move on. 
         5)    If yes, check if there an another instance of King or Queen in the stored cards, 
         6)       If yes, switch around the paired Cards so that they are paired with a different instance of the King or Queen card
         7)       If no, then remove all cards from the storage (cardsOfEachRank)
         5) create melds from all the collected cards and return a vector of the melds 
Assistance Received: none
********************************************************************* */
get_same_suit_melds(Meld, Hand, MeldsPlayed, Suit, StartingRankIndex, NumOfCardsInMeld, SameSuitMelds) :-
   collect_cards_of_each_rank(Meld, Hand, MeldsPlayed, Suit, StartingRankIndex, NumOfCardsInMeld, CardsOfEachRank),
   ensure_royal_marriage_melds_contain_card_from_hand(Meld, MeldsPlayed, CardsOfEachRank, NewCardsOfEachRank),
   create_melds_from_eligible_cards(NewCardsOfEachRank, SameSuitMelds).



/* *********************************************************************
Rule Name: collect_cards_of_each_rank/7
Purpose: gets the list of lists of cards of each rank that comprise the given same suit meld, where none of the cards have been used previously to 
            create an instance of the given meld type
Parameters:
   Input:
      Meld: the meld type whose instances are to be returned
      Hand: the hand of the player 
      MeldsPlayed: the melds played by the player
      Suit: the suit of the Same Suit meld
      StartingRankIndex: the highest rank comprising the meld
      NumOfCardsInMeld: the number of cards in the meld
   Output: 
      CardsOfEachRank: List of the lists of all the cards of the given suit of each rank that comprise the meld. These cards also have not been used
                           to create a Meld of the same type before
   Algorithm:
         1) Get a list of all the cards from hand of the first rank and of the given suit
         2) Remove all those cards in the list that have already been used to create the same meld
         3) Repeat the process for all the other required ranks
         4) Return the list of the lists of all the cards of each rank
Assistance Received: none
********************************************************************* */
collect_cards_of_each_rank(_, _, _, _, _, 0, []).

collect_cards_of_each_rank(Meld, Hand, MeldsPlayed, Suit, StartingRankIndex, NumOfCardsInMeld, CardsOfEachRank) :-
   rank_index(Rank, StartingRankIndex),
   get_cards_by_rank_and_suit(Hand, Rank, Suit, AllCardsOfGivenRankAndSuit),
   filter_out_used_cards(Meld, MeldsPlayed, AllCardsOfGivenRankAndSuit, FilteredCards),
   NewRankIndex is StartingRankIndex - 1,
   NewNumOfCardsInMeld is NumOfCardsInMeld - 1,
   collect_cards_of_each_rank(Meld, Hand, MeldsPlayed, Suit, NewRankIndex, NewNumOfCardsInMeld, UnfinishedCardsOfEachRank),
   CardsOfEachRank = [FilteredCards | UnfinishedCardsOfEachRank].



/* *********************************************************************
Rule Name: get_same_rank_melds/7
Purpose: gets each possible same rank melds of the given rank from the given hand.
            Same rank melds are those melds that have cards with the same rank but of different suit,
            for example: Four Kings and Four Jacks
Parameters:
   Input:
      Meld: the meld type whose instances are to be returned
      Hand: the hand of the player 
      MeldsPlayed: the melds played by the player
      Rank: the rank of the Same Rank meld
   Output: 
      SameRankMelds: List of all the same rank melds of given type in the player's hand
   Algorithm:
         1) loop through all the cards in hand to get the cards of each suit with the required rank
         2) If a card has not already been used to create the given meld, store it
         3) create melds from all the collected cards and return a list of the melds 
Assistance Received: none
********************************************************************* */
get_same_rank_melds(Meld, Hand, MeldsPlayed, Rank, SameRankMelds) :-
   collect_cards_of_each_suit(Meld, Hand, MeldsPlayed, Rank, CardsOfEachSuit),
   create_melds_from_eligible_cards(CardsOfEachSuit, SameRankMelds).


/* *********************************************************************
Rule Name: collect_cards_of_each_suit/5
Purpose: gets the list of lists of cards of each suit that comprise the given same rank meld, where none of the cards have been used previously to 
            create an instance of the given meld type
Parameters:
   Input:
      Meld: the meld type whose instances are to be returned
      Hand: the hand of the player 
      MeldsPlayed: the melds played by the player
      Ranks: the rank of the Same ranks meld
   Output: 
      CardsOfEachSuit: List of the lists of all the cards of the given rank of each suit that comprise the meld. These cards also have not been used
                           to create a Meld of the same type before
   Algorithm:
         1) Get a list of all the cards from hand of the first suit and of the given rank
         2) Remove all those cards in the list that have already been used to create the same meld
         3) Repeat the process for all the other required suits
         4) Return the list of the lists of all the cards of each suit
Assistance Received: none
********************************************************************* */
collect_cards_of_each_suit(Meld, Hand, MeldsPlayed, Rank, CardsOfEachSuit) :-
   num_of_suits_in_game(NumOfSuits),
   collect_cards_of_each_suit(Meld, Hand, MeldsPlayed, Rank, NumOfSuits, CardsOfEachSuit).

collect_cards_of_each_suit(_, _, _, _, 0, []).

collect_cards_of_each_suit(Meld, Hand, MeldsPlayed, Rank, Counter, CardsOfEachSuit) :-
   SuitIndex is Counter - 1,
   suit_index(Suit, SuitIndex),
   %get all the cards of given rank of the current value of Suit
   get_cards_by_rank_and_suit(Hand, Rank, Suit, AllCardsOfGivenRankAndSuit),

   %from these cards, filter out cards that have already been used to create the meld
   filter_out_used_cards(Meld, MeldsPlayed, AllCardsOfGivenRankAndSuit, FilteredCards),
   NewCounter is Counter - 1,
   collect_cards_of_each_suit(Meld, Hand, MeldsPlayed, Rank, NewCounter, UnfinishedCardsOfEachSuit),
   CardsOfEachSuit = [FilteredCards | UnfinishedCardsOfEachSuit].



/* *********************************************************************
Rule Name: filter_out_used_cards/4
Purpose: removes those cards from a list of cards that have already been used to create the given meld
Parameters:
   Input:
      Meld: the meld type that is to be checked for
      MeldsPlayed: the melds played by the player 
      CardList: the list of cards from which previously used melds are to be filtered out
   Output: 
      FilteredCards: List of cards without cards that have already been used to create the meld
   Algorithm:
         1) If the card has not already been used to create the meld, then include the card in the card list to be returned
         2) If the card has already been used to create the meld, then skip the card
Assistance Received: none
********************************************************************* */
filter_out_used_cards(_, _, [], []).

filter_out_used_cards(Meld, MeldsPlayed, [FirstCard | RemainingCards], FilteredCards) :-
   \+ (card_is_used_by_meld(MeldsPlayed, FirstCard, Meld)),
   filter_out_used_cards(Meld, MeldsPlayed, RemainingCards, IncompletedFilteredCards),
   FilteredCards = [FirstCard | IncompletedFilteredCards].

filter_out_used_cards(Meld, MeldsPlayed, [FirstCard | RemainingCards], FilteredCards) :-
   card_is_used_by_meld(MeldsPlayed, FirstCard, Meld),
   filter_out_used_cards(Meld, MeldsPlayed, RemainingCards, FilteredCards).


/* *********************************************************************
Rule Name: ensure_royal_marriage_melds_contain_card_from_hand/4
Purpose: Rearranges (or removes) a list containings a list of King of TrumpSuit and a list of Queen of Trump Suit so that the
         adjacent cards contain at least one new card from hand to create a valid royal marriage meld
Parameters:
   Input:
      Meld: the meld type
      MeldsPlayed: the melds played by the player 
      CardsOfEachRank: the list containing the list of King of TrumpSuit cards and the list of Queen of Trump Suit cards
   Output: 
      NewCardsOfEachRank: List of list of King of Trump Suit and Queen of Trump Suit cards where adjacent cards contain 
            at least one new card from hand to create a valid royal marriage meld
   Algorithm:
         1) If there one or more King of Trump Suit and Queen of Trump Suit cards 
               2) if adjacted king of trump suit and queen of trump suit have been together used to create the same meld
                  3) Switch the card pairings
Assistance Received: none
********************************************************************* */
/* if the meld is a royal marriage, we have to make sure that two cards that have already been used to create a meld are not 
   being reused to created another meld */
ensure_royal_marriage_melds_contain_card_from_hand(royal_marriage, MeldsPlayed, [KingCards | [QueenCards]], NewCardsOfEachRank) :-
   %make sure there is at least one King card
   length(KingCards, KingCardsAmount),
   KingCardsAmount > 0, 

   %make sure there is at least one Queen card 
   length(QueenCards, QueenCardsAmount),
   QueenCardsAmount > 0,

   %if the first king card and the first Queen card have already been used together to create a meld
   get_card_by_position(KingCards, 0, FirstKingCard),
   get_card_by_position(QueenCards, 0, FirstQueenCard),
   cards_are_used_for_same_meld(MeldsPlayed, [FirstKingCard, FirstQueenCard]),
   switch_card_pairings([KingCards, QueenCards], NewCardsOfEachRank).

ensure_royal_marriage_melds_contain_card_from_hand(royal_marriage, _, CardsOfEachRank, CardsOfEachRank).

%if any meld besides royal marriage, then we can be sure that the meld contains at least one new card from hand
ensure_royal_marriage_melds_contain_card_from_hand(Meld, _, CardsOfEachRank, CardsOfEachRank) :-
   Meld \= royal_marriage.



/* *********************************************************************
Rule Name: switch_card_pairings/2
Purpose: Switches the card pairings in a list containg a list of King cards and Queen cards. Returns an empty list if there are is 
            one instance each of both cards
Parameters:
   Input:
      Meld: the meld type
      MeldsPlayed: the melds played by the player 
      CardsOfEachRank: the list containing the list of King of TrumpSuit cards and the list of Queen of Trump Suit cards
   Output: 
      NewCardsOfEachRank: List of list of King of Trump Suit and Queen of Trump Suit cards where adjacent cards have been switched
                        to form new pairings of King of Trump Suit and Queen of Trump Suit
   Algorithm:
         1) If there are two king cards, then return the list by switching the first king card with the second
         2) If there are two queen cards, then return the list by switching the first queen card with the second
         3) If there is only king card and one queen card, return an empty list
Assistance Received: none
********************************************************************* */
switch_card_pairings([KingCards | [QueenCards]], NewCardsOfEachRank) :-
   %if there is more than one King card
   length(KingCards, KingCardsAmount),
   KingCardsAmount > 1, 
   [KingCard1 | [KingCard2]] = KingCards,
   NewKingCardsList = [KingCard2, KingCard1],
   NewCardsOfEachRank = [NewKingCardsList, QueenCards].

switch_card_pairings([KingCards | [QueenCards]], NewCardsOfEachRank) :-
   %if there is more than one Queen card
   length(QueenCards, QueenCardsAmount),
   QueenCardsAmount > 1, 
   [QueenCard1 | [QueenCard2]] = QueenCards,
   NewQueenCardsList = [QueenCard2, QueenCard1],
   NewCardsOfEachRank = [KingCards, NewQueenCardsList].

%if there are only one instances each of King and Queen cards, remove them both
switch_card_pairings(_, [[], []]).
   

/* *********************************************************************
Rule Name: create_melds_from_eligible_cards/2
Purpose: Get a list containing lists of each type of card that can be used to create a given meld, and creates a list 
            of melds from that list
Parameters:
   Input:
      MeldEligibleCards: the list containing lists of each type of card that can be used to create a given meld
   Output: 
      AllPossibleMelds: a list of melds generated from the given list
   Algorithm:
         1) Make melds from the all the cards in the first positions of the lists of cards in the list of lists
         2) Add the created meld to the list of all possible melds
         3) Remove the first cards from all the lists in the list of lists
         4) repeat from the first step until any one of the list of lists runs out of cards
         5) return the created list of melds 
         
Assistance Received: none
********************************************************************* */
create_melds_from_eligible_cards(MeldEligibleCards, AllPossibleMelds) :-
   find_size_of_smallest_list(MeldEligibleCards, NumOfPossibleMelds),
   create_melds_from_eligible_cards(MeldEligibleCards, NumOfPossibleMelds, AllPossibleMelds).


%if number of possible lists is zero, then all possible melds is comprised of an empty list
create_melds_from_eligible_cards(_, 0, []).

create_melds_from_eligible_cards(MeldEligibleCards, NumOfPossibleMelds, AllPossibleMelds) :-   
   remove_first_items_of_each_list(MeldEligibleCards, NewMeld, RemainingMeldEligibleCards),
   NewNumOfPossibleMelds is NumOfPossibleMelds - 1,
   create_melds_from_eligible_cards(RemainingMeldEligibleCards, NewNumOfPossibleMelds, RemainingMelds),
   AllPossibleMelds = [NewMeld | RemainingMelds].


/* *********************************************************************
Rule Name: find_size_of_smallest_list/2
Purpose: Finds the size of the list that has the smallest size from a list of lists
Parameters:
   Input:
      ListsOfLists: the list containing the list of lists
   Output: 
      SizeOfSmallestList: the size of the smallest list in the list of lists
         
Assistance Received: none
********************************************************************* */
find_size_of_smallest_list([], undefined).

find_size_of_smallest_list([FirstList | RemainingLists], SizeOfSmallestList) :-
   length(FirstList, FirstListSize),
   find_size_of_smallest_list(RemainingLists, FirstListSize, SizeOfSmallestList).

find_size_of_smallest_list([], ProvisionalSmallestSize, ProvisionalSmallestSize).

find_size_of_smallest_list([FirstList | RemainingLists], ProvisionalSmallestSize, SizeOfSmallestList) :-
   length(FirstList, FirstListSize),
   FirstListSize < ProvisionalSmallestSize,
   find_size_of_smallest_list(RemainingLists, FirstListSize, SizeOfSmallestList).

find_size_of_smallest_list([FirstList | RemainingLists], ProvisionalSmallestSize, SizeOfSmallestList) :-
   length(FirstList, FirstListSize),
   FirstListSize >= ProvisionalSmallestSize,
   find_size_of_smallest_list(RemainingLists, ProvisionalSmallestSize, SizeOfSmallestList).
   


/* *********************************************************************
Rule Name: remove_first_items_of_each_list/3
Purpose: Remove the first items from each list in the given list of lists
Parameters:
   Input:
      List: the list containing the list of lists
   Output: 
      FirstItemOfEachList: the list containing the first item of each list in the list of lists
      NewListOfLists: The list of lists where the first item of each list has been removed
         
Assistance Received: none
********************************************************************* */
remove_first_items_of_each_list([], [], []).

remove_first_items_of_each_list([FirstList | RemainingLists], FirstItemsOfEachList, NewListOfLists) :-
   [FirstItemOfFirstList | NewFirstList ] = FirstList,
   remove_first_items_of_each_list(RemainingLists, FirstItemsOfRemainingLists, NewListOfRemainingLists),
   FirstItemsOfEachList = [FirstItemOfFirstList | FirstItemsOfRemainingLists],
   NewListOfLists = [NewFirstList | NewListOfRemainingLists].


/* *********************************************************************************************************
Clauses to help with AI logic relating to melds
*********************************************************************************************************** */

/* *********************************************************************
Rule Name: compare_hands_for_melds/5
Purpose: Compare two hands to see which hand has the better melds
Parameters:
   Input:
      Hand1: the first hand to be compared
      Hand2: the second hand to be compared
      MeldsPlayed: the melds played by he player
      TrumpSuit: the trump suit for the round
   Output: 
      Winner: the winning hand
Assistance Received: none
********************************************************************* */
compare_hands_for_melds(Hand1, Hand2, MeldsPlayed, TrumpSuit, Winner) :-
   %get the list of potential points from each hand and compare which one has the better points list
   potential_meld_points_from_hand(Hand1, MeldsPlayed, TrumpSuit, Hand1Points),
   potential_meld_points_from_hand(Hand2, MeldsPlayed, TrumpSuit, Hand2Points),
   compare_meld_points_list(Hand1Points, Hand2Points, Winner).


/* *********************************************************************
Rule Name: compare_meld_points_list/3
Purpose: Compare two lists of points (where the lists of points are in sorted order)
Parameters:
   Input:
      PointsList1: the first points list to be compared
      PointsList2: the second points list to be compared
   Output: 
      WinningHand: atom representing which hand corresponds to the better points list
   Algorithm:
      1) Choose that points list from the two that has the meld that yields the most points
      2) If it is a draw, choose that list from the two that has the melds, which altogether yield the most points
      3) If even that is a draw, compare the second-highest point of pointslist1 with that of pointslist2
      4) If that is a draw, check 3rd highest meld points of each list, 4th highest, 5th highest, and so on
      5) Do the above step until melds have run out
      6) If it is still a draw, declare a draw by returning 0
Assistance Received: none
********************************************************************* */
compare_meld_points_list([], [], -1).

compare_meld_points_list([FirstPoint1 | _ ], [FirstPoint2 | _ ], first_hand) :- 
   %if the highest point in the first list is greater, than the first is better
   FirstPoint1 > FirstPoint2.

compare_meld_points_list([FirstPoint1 | _ ], [FirstPoint2 | _ ], second_hand) :- 
   %if the highest point in the second list is greater, than the second hand is better
   FirstPoint1 < FirstPoint2.

compare_meld_points_list(PointsList1, PointsList2, Winner) :- 
   [FirstPoint1 | _ ] = PointsList1,
   [FirstPoint2 | _ ] = PointsList2,
   %if the highest point in the first and second lists are the same, then that list is better which as greater cumulative points
   FirstPoint1 = FirstPoint2,

   %then see which list has the greater cumulative score
   add_all_points_in_list(PointsList1, CumulativePoints1),
   add_all_points_in_list(PointsList2, CumulativePoints2),

   %checking that both Points List do not have the same cumlative score
   compare_cumulative_meld_points(CumulativePoints1, CumulativePoints2, BestHand),
   %if not a draw, then the winning hand is decided
   (\+ BestHand = draw),
   Winner = BestHand.

compare_meld_points_list(PointsList1, PointsList2, Winner) :- 
   %if the highest point in the first and second lists are the same and if both Points List have the same cumulative score
   compare_each_point_values_in_list(PointsList1, PointsList2, Winner).



/* *********************************************************************
Rule Name: add_all_points_in_list/2
Purpose: Adds all the points in a list of points
Parameters:
   Input:
      PointsList: the list of points
   Output: 
      CumulativePoints: the sum of all the points in the list of points
   Algorithm:
      1) Choose that points list from the two that has the meld that yields the most points
      2) If it is a draw, choose that list from the two that has the melds, which altogether yield the most points
      3) If even that is a draw, compare the second-highest point of pointslist1 with that of pointslist2
      4) If that is a draw, check 3rd highest meld points of each list, 4th highest, 5th highest, and so on
      5) Do the above step until melds have run out
      6) If it is still a draw, declare a draw by returning 0
Assistance Received: none
********************************************************************* */
add_all_points_in_list([], 0).

add_all_points_in_list([FirstPoints | RemainingPoints], CumulativePoints) :-
   add_all_points_in_list(RemainingPoints, ProvisionalCumulativePoints),
   CumulativePoints is FirstPoints + ProvisionalCumulativePoints.


/* *********************************************************************
Rule Name: compare_cumulative_meld_points/3
Purpose: Finds out which hand has the higher cumulative meld points given the cumulative meld points of each hand
Parameters:
   Input:
      CumulativePoints1: the cumulative meld points of the first hand
      CumulativePoints2: the cumulative meld points of the second hand
   Output: 
      WinningHand: the hand with the higher cumulative meld points
Assistance Received: none
********************************************************************* */
compare_cumulative_meld_points(CumulativePoints1, CumulativePoints2, first_hand) :-
   CumulativePoints1 > CumulativePoints2.

compare_cumulative_meld_points(CumulativePoints1, CumulativePoints2, second_hand) :-
   CumulativePoints1 < CumulativePoints2.

compare_cumulative_meld_points(_, _, CumulativePoints1, CumulativePoints2, draw) :-
   CumulativePoints1 = CumulativePoints2.


/* *********************************************************************
Rule Name: compare_each_point_values_in_list/3
Purpose: Finds out which hand has the better meld point values given the meld points list of each hand
Parameters:
   Input:
      PointsList1: the meld points of the first hand
      PointsList2: the meld points of the second hand
   Output: 
      WinningHand: the hand with the meld points list with higher values
Assistance Received: none
********************************************************************* */
compare_each_point_values_in_list([], [], draw).

compare_each_point_values_in_list(_, [], first_hand).

compare_each_point_values_in_list([], _, second_hand).

compare_each_point_values_in_list([FirstPoint1 | _ ], [FirstPoint2 | _ ], first_hand) :-
   FirstPoint1 > FirstPoint2.

compare_each_point_values_in_list([FirstPoint1 | _ ], [FirstPoint2 | _ ], second_hand) :-
   FirstPoint1 < FirstPoint2.

compare_each_point_values_in_list([FirstPoint1 | RemainingPoints1], [FirstPoint2 | RemainingPoints2], BestHand) :-
   FirstPoint1 = FirstPoint2,
   %compare the next of pair of adjacent points in the two lists
   compare_each_point_values_in_list(RemainingPoints1, RemainingPoints2, BestHand).



/* *********************************************************************
Rule Name: potential_meld_points_from_hand/4
Purpose: Generates the list of possible meld points from a given hand
Parameters:
   Input:
      Hand: the hand of the player
      MeldsPlayed: the melds already played by the player
      TrumpSuit: the trump suit for the round
   Output: 
      Points: The sorted list of points obtaininable from the hand
Assistance Received: none
********************************************************************* */
potential_meld_points_from_hand(Hand, MeldsPlayed, TrumpSuit, Points) :-
   counts_of_each_meld_in_hand(Hand, MeldsPlayed, TrumpSuit, CountsOfEachMeld),
   convert_melds_to_points(CountsOfEachMeld, UnsortedPoints),
   sort(0, @>=, UnsortedPoints, Points).


/* *********************************************************************
Rule Name: counts_of_each_meld_in_hand/4
Purpose: Generates the number of times each meld type occurs in a hand 
Parameters:
   Input:
      Hand: the hand of the player
      MeldsPlayed: the melds already played by the player
      TrumpSuit: the trump suit for the round
   Output: 
      CountsOfEachMeld: The list containing the counts of each meld type in the hand
Assistance Received: none
********************************************************************* */
counts_of_each_meld_in_hand(Hand, MeldsPlayed, TrumpSuit, CountsOfEachMeld) :-
   counts_of_each_meld_in_hand(Hand, MeldsPlayed, TrumpSuit, 0, CountsOfEachMeld).

counts_of_each_meld_in_hand(_, _, _, StartingMeldIndex, []) :-
   num_of_melds_in_game(NumOfMelds),
   StartingMeldIndex >= NumOfMelds.

counts_of_each_meld_in_hand(Hand, MeldsPlayed, TrumpSuit, StartingMeldIndex, CountsOfEachMeld) :-
   meld_index(MeldType, StartingMeldIndex),
   get_melds_from_hand(Hand, MeldsPlayed, MeldType, TrumpSuit, MeldsOfGivenType),
   length(MeldsOfGivenType, NumOfMelds),
   NextIndex is StartingMeldIndex + 1,
   counts_of_each_meld_in_hand(Hand, MeldsPlayed, TrumpSuit, NextIndex, IncompleteCountsOfEachMeld),
   CountsOfEachMeld = [NumOfMelds | IncompleteCountsOfEachMeld].


/* *********************************************************************
Rule Name: total_number_of_melds_in_hand/4
Purpose: Generates the total number of possible melds in the hand
Parameters:
   Input:
      Hand: the hand of the player
      MeldsPlayed: the melds already played by the player
      TrumpSuit: the trump suit for the round
   Output: 
      TotalNumOfMeldsInHand: The total number of melds in possible in the hand
Assistance Received: none
********************************************************************* */
total_number_of_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, TotalNumOfMeldsInHand) :-
   total_number_of_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, 0, TotalNumOfMeldsInHand).

total_number_of_melds_in_hand(_, _, _, StartingMeldIndex, 0) :-
   num_of_melds_in_game(NumOfMelds),
   StartingMeldIndex >= NumOfMelds.

total_number_of_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, StartingMeldIndex, TotalNumOfMeldsInHand) :-
   meld_index(MeldType, StartingMeldIndex),
   get_melds_from_hand(Hand, MeldsPlayed, MeldType, TrumpSuit, MeldsOfGivenType),
   length(MeldsOfGivenType, NumOfMelds),
   NextIndex is StartingMeldIndex + 1,
   total_number_of_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, NextIndex, NumOfRemainingMeldsInHand),
   TotalNumOfMeldsInHand is NumOfMelds + NumOfRemainingMeldsInHand.



/* *********************************************************************
Rule Name: create_meld/3
Purpose: Determines the meld type of a list containing cards. If the cards do not comprise a valid meld,
            then returns the atom indicating it is not a valid meld
Parameters:
   Input:
      Cards: the list of cards that comprise the potential meld
      TrumpSuit: the trump suit for the round
   Output: 
      MeldType: the type of the meld created by the list of cards
Assistance Received: none
********************************************************************* */

create_meld([], _, not_a_valid_meld).

create_meld(Cards, _, not_a_valid_meld) :-
   length(Cards, CardSize),
   CardSize = 3.

create_meld(Cards, _, not_a_valid_meld) :-
   length(Cards, CardSize),
   CardSize > 5.


create_meld(Cards, TrumpSuit, MeldType) :-
   length(Cards, CardSize),
   CardSize = 4,
   create_meld_from_rank_sorted_cards(Cards, TrumpSuit, CalculatedMeldType),
   MeldType = CalculatedMeldType.

create_meld(Cards, TrumpSuit, MeldType) :-
   length(Cards, CardSize),
   CardSize = 4,
   %if there are four cards in the list, it is potentially a Four Aces, Four Kings, Four Queens, or Four Jacks
   create_meld_from_rank_sorted_cards(Cards, TrumpSuit, CalculatedMeldType),
   MeldType = CalculatedMeldType.

create_meld(Cards, TrumpSuit, MeldType) :-
   length(Cards, CardSize),
   CardSize \= 0,
   CardSize \= 3,
   CardSize =< 5,
   %if there are 1, 2, or 5 cards in the list then it is probably a meld in which the ranks of the card are not the same as each other
   %so sort the cards by rank to make it easier to determine the meld
   sort_cards_by_rank(Cards, RankSortedCards),
   create_meld_from_rank_sorted_cards(RankSortedCards, TrumpSuit, CalculatedMeldType),
   MeldType = CalculatedMeldType.

create_meld(Cards, TrumpSuit, not_a_valid_meld) :-
   sort_cards_by_rank(Cards, RankSortedCards),
   \+ (create_meld_from_rank_sorted_cards(RankSortedCards, TrumpSuit, _)).


/* *********************************************************************
Rule Name: create_meld_from_rank_sorted_cards/3
Purpose: Determines the meld type of a list containing cards in order sorted by rank. If the cards do not comprise a valid meld,
            then the clause fails
Parameters:
   Input:
      Cards: the list of cards that comprise the potential meld
      TrumpSuit: the trump suit for the round
   Output: 
      MeldType: the type of the meld created by the list of cards
Assistance Received: none
********************************************************************* */

create_meld_from_rank_sorted_cards([Card], TrumpSuit, dix) :-
   %if there is one card and it is Nine of Trump Suit
   get_card_rank(Card, nine),
   get_card_suit(Card, TrumpSuit).

create_meld_from_rank_sorted_cards([Card1, Card2], _, pinochle) :-
   %if there are two cards and they are jack of diamonds and queen of spades
   get_card_rank(Card1, jack),
   get_card_suit(Card1, diamonds),
   get_card_rank(Card2, queen),
   get_card_suit(Card2, spades).

create_meld_from_rank_sorted_cards([Card1, Card2], TrumpSuit, royal_marriage) :-
   %if there are two cards and they are Queen of TrumpSuit and King of TrumpSuit
   get_card_rank(Card1, queen),
   get_card_rank(Card2, king),
   get_card_suit(Card1, TrumpSuit),
   get_card_suit(Card2, TrumpSuit).

create_meld_from_rank_sorted_cards([Card1, Card2], TrumpSuit, marriage) :-
   %if there are two cards and they are King and Queen of same suit
   get_card_suit(Card1, Card1Suit),
   Card1Suit \= TrumpSuit,
   get_card_suit(Card2, Card1Suit),
   get_card_rank(Card1, queen),
   get_card_rank(Card2, king).

create_meld_from_rank_sorted_cards([Card1, Card2, Card3, Card4], _, four_aces) :-
   %if there are four cards and they are all aces of different suits
   get_card_rank(Card1, ace),
   get_card_rank(Card2, ace),
   get_card_rank(Card3, ace),
   get_card_rank(Card4, ace),
   sort_cards_by_suit([Card1, Card2, Card3, Card4], [NewCard1, NewCard2, NewCard3, NewCard4]),
   get_card_suit(NewCard1, clubs),
   get_card_suit(NewCard2, diamonds),
   get_card_suit(NewCard3, hearts),
   get_card_suit(NewCard4, spades).

create_meld_from_rank_sorted_cards([Card1, Card2, Card3, Card4], _, four_kings) :-
   %if there are four cards and they are all kings of different suits
   get_card_rank(Card1, king),
   get_card_rank(Card2, king),
   get_card_rank(Card3, king),
   get_card_rank(Card4, king),
   sort_cards_by_suit([Card1, Card2, Card3, Card4], [NewCard1, NewCard2, NewCard3, NewCard4]),
   get_card_suit(NewCard1, clubs),
   get_card_suit(NewCard2, diamonds),
   get_card_suit(NewCard3, hearts),
   get_card_suit(NewCard4, spades).

create_meld_from_rank_sorted_cards([Card1, Card2, Card3, Card4], _, four_queens) :-
   %if there are four cards and they are all queens of different suits
   get_card_rank(Card1, queen),
   get_card_rank(Card2, queen),
   get_card_rank(Card3, queen),
   get_card_rank(Card4, queen),
   sort_cards_by_suit([Card1, Card2, Card3, Card4], [NewCard1, NewCard2, NewCard3, NewCard4]),
   get_card_suit(NewCard1, clubs),
   get_card_suit(NewCard2, diamonds),
   get_card_suit(NewCard3, hearts),
   get_card_suit(NewCard4, spades).

create_meld_from_rank_sorted_cards([Card1, Card2, Card3, Card4], _, four_jacks) :-
   %if there are four cards and they are all jacks of different suits
   get_card_rank(Card1, jack),
   get_card_rank(Card2, jack),
   get_card_rank(Card3, jack),
   get_card_rank(Card4, jack),
   sort_cards_by_suit([Card1, Card2, Card3, Card4], [NewCard1, NewCard2, NewCard3, NewCard4]),
   get_card_suit(NewCard1, clubs),
   get_card_suit(NewCard2, diamonds),
   get_card_suit(NewCard3, hearts),
   get_card_suit(NewCard4, spades).

create_meld_from_rank_sorted_cards([Card1, Card2, Card3, Card4, Card5], TrumpSuit, flush) :-
   %if there are fives cards and they comprise of jack, queen, king, ten, and ace of trump suit
   get_card_rank(Card1, jack),
   get_card_rank(Card2, queen),
   get_card_rank(Card3, king),
   get_card_rank(Card4, ten),
   get_card_rank(Card5, ace),
   get_card_suit(Card1, TrumpSuit),
   get_card_suit(Card2, TrumpSuit),
   get_card_suit(Card3, TrumpSuit),
   get_card_suit(Card4, TrumpSuit),
   get_card_suit(Card5, TrumpSuit).
   


/* *********************************************************************
Rule Name: is_a_playable_meld/3
Purpose: Succeeds if the given list of cards form a playable meld 
Parameters:
   Input:
      MeldsPlayed: the melds already played by the player
      Cards: the cards to be determined for meld playability
      TrumpSuit: the trump suit for the round
   Output: 
Assistance Received: none
********************************************************************* */
is_a_playable_meld(_, [], _) :-
   false.

is_a_playable_meld(_, Cards, TrumpSuit) :-
   create_meld(Cards, TrumpSuit, not_a_valid_meld),
   false.

is_a_playable_meld(MeldsPlayed, Cards, TrumpSuit) :-
   create_meld(Cards, TrumpSuit, Meld),
   Meld \= not_a_valid_meld,
   \+ (at_least_one_card_is_used_by_meld(MeldsPlayed, Cards, Meld)).



/* *********************************************************************
Rule Name: at_least_one_card_is_used_by_meld/3
Purpose: Succeeds if at least one of the cards in the given list of cards has been used to create an instance of the given meld
Parameters:
   Input:
      MeldsPlayed: the melds already played by the player
      Cards: the cards to be determined for meld playability
      Meld: the trump suit for the round
   Output: 
Assistance Received: none
********************************************************************* */

at_least_one_card_is_used_by_meld(MeldsPlayed, Cards, Meld) :-
   length(Cards, NumOfCards),
   at_least_one_card_is_used_by_meld(MeldsPlayed, Cards, Meld, NumOfCards).

at_least_one_card_is_used_by_meld(MeldsPlayed, [FirstCard | _], Meld, Counter) :-
   Counter > 0,
   card_is_used_by_meld(MeldsPlayed, FirstCard, Meld).

at_least_one_card_is_used_by_meld(MeldsPlayed, [FirstCard | RemainingCards], Meld, Counter) :-
   Counter > 0,
   \+ (card_is_used_by_meld(MeldsPlayed, FirstCard, Meld)),
   NewCounter is Counter - 1,
   at_least_one_card_is_used_by_meld(MeldsPlayed, RemainingCards, Meld, NewCounter).


/* *********************************************************************
Rule Name: generate_empty_melds_storage/1
Purpose: Generates a list of 9 empty lists to be used for storing melds in a game of pinochle
Parameters:
   Input:
   Output: 
         EmptyMeldsStorage: the generate melds storage
Assistance Received: none
********************************************************************* */
generate_empty_melds_storage(EmptyMeldsStorage) :-
   num_of_melds_in_game(NumOfMelds),
   generate_list_containing_empty_lists(NumOfMelds, EmptyMeldsStorage).


/* *********************************************************************
Rule Name: generate_list_containing_empty_lists/2
Purpose: Generates a list of containing the specified number of empty lists
Parameters:
   Input:
      NumOfInnerListsToAdd: the number of empty lists to be generated
   Output: 
         ListOfLists: the generate list of empty lists
Assistance Received: none
********************************************************************* */
generate_list_containing_empty_lists(0, []).

generate_list_containing_empty_lists(NumOfInnerListsToAdd, ListOfLists) :-
   NewNumOfInnerListsToAdd is NumOfInnerListsToAdd - 1,
   generate_list_containing_empty_lists(NewNumOfInnerListsToAdd, IncompleteListOfLists),
   ListOfLists = [[] | IncompleteListOfLists].

