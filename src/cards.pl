:- module(cards,[
                  num_of_ranks_in_game/1, 
                  num_of_suits_in_game/1,
                  num_of_each_card_type/1,
                  num_of_unique_cards_in_game/1,
                  num_of_cards_in_game/1,
                  get_card_rank/2,
                  get_card_suit/2,
                  rank_string/2,
                  suit_string/2,
                  rank_index/2,
                  suit_index/2,
                  card_string/2,
                  card_or_suit_string/2,
                  card_with_higher_rank/3,
                  card_with_higher_suit/3,
                  card_with_less_rank/3,
                  card_with_same_or_less_rank/3,
                  card_with_same_or_less_suit/3,
                  compare_ranks/3,
                  card_points/2,
                  card_id_to_atom/2,
                  card_id_to_asterisked_atom/2,
                  card_atom_to_id/2,
                  card_atom_type/2,
                  get_card_id_from_rank_and_suit/3
               ]).

/* ********************************************************************************************************
Clauses that denote the definition of ranks, suits and cards, and the set up of the deck in the game
******************************************************************************************************** */


/* *
   Number of ranks in the game
*/
num_of_ranks_in_game(6).

/* 
Number of suits in the game
 */
num_of_suits_in_game(4).

/* 
How many times is each card repeated
 */
num_of_each_card_type(2).


/* 
The index of each rank, needed to decide which rank beats which rank
 */
rank_index(nine, 0).
rank_index(jack, 1).
rank_index(queen, 2).
rank_index(king, 3).
rank_index(ten, 4).
rank_index(ace, 5).


/* *********************************************************************
Rule Name: num_of_unique_cards_in_game
Purpose: Calculates the number of unique cads in the game
Parameters:
   Input:
   Output:
         NumOfUniqueCards
Assistance Received: none
********************************************************************* */
num_of_unique_cards_in_game(NumOfUniqueCards) :-
   num_of_ranks_in_game(NumOfRanks),
   num_of_suits_in_game(NumOfSuits),
   NumOfUniqueCards is NumOfRanks * NumOfSuits.

/* *********************************************************************
Rule Name: num_of_unique_cards_in_game
Purpose: Calculates the total number of cads in the game
Parameters:
   Input:
   Output: 
      NumOfCards
Assistance Received: none
********************************************************************* */
num_of_cards_in_game(NumOfCards) :-
   num_of_unique_cards_in_game(NumOfUniqueCards),
   num_of_each_card_type(NumOfEachCardType),
   NumOfCards is NumOfUniqueCards * NumOfEachCardType.


/* *********************************************************************
Rule Name: num_of_unique_cards_in_game
Purpose: Calculates the total number of cads in the game
Parameters:
   Input:
      Card: The card (represented by an integer)
   Output: 
      Rank: THe rank of the card

Assistance Received: none
********************************************************************* */
get_card_rank(Card, Rank) :-
   /* 
   There are 6 ranks and they are listed in the following order, in accordance with their value: nine, jack, queen, king, ten, ace
   The rank of each card in the game is determined as followed: 
     [Note: below, 
                  0 <= x <= (num_of_suits_in_game * num_of_each_card_type - 1)
     nine: Any card with Id denoted by (num_of_ranks_in_game * x)
     jack: Any card with Id denoted by (num_of_ranks_in_game * x + 1)
     queen: Any card with Id denoted by (num_of_ranks_in_game * x + 2)
     king: Any card with Id denoted by (num_of_ranks_in_game * x + 3)
     ten: Any card with Id denoted by (num_of_ranks_in_game * x + 4)
     ace: Any card with Id denoted by (num_of_ranks_in_game * x + 5)
    */
   num_of_ranks_in_game(NumOfRanks),
   Remainder is mod(Card, NumOfRanks),
   rank_index(Rank, Remainder).



/*
There are 4 suits and they are listed in the following order, in accordance with their value: clubs, diamonds, hearts, spades
The suit of each card in the game is determined as followed: 
  [Note: below, 
               num_of_unique_cards_in_game = num_of_ranks_in_game * num_of_suits_in_game
               0 <= x <= (num_of_each_card_type - 1)]
  clubs: Any card with Id such that (x * num_of_unique_cards_in_game) <= Id <= (x * num_of_unique_cards_in_game + num_of_ranks_in_game - 1)
  diamonds: Any card with Id such that (x * num_of_unique_cards_in_game + num_of_ranks_in_game) <= Id <= (x * num_of_unique_cards_in_game + 2 * num_of_ranks_in_game - 1)
  hearts: Any card with Id such that (x * num_of_unique_cards_in_game + 2 * num_of_ranks_in_game) <= Id <= (x * num_of_unique_cards_in_game + 3 * num_of_ranks_in_game - 1)
  spades: Any card with Id such that (x * num_of_unique_cards_in_game + 3 * num_of_ranks_in_game) <= Id <= (x * num_of_unique_cards_in_game + 4 * num_of_ranks_in_game - 1)
*/

/* *********************************************************************
Rule Name: get_card_suit/2
Purpose: Wrapper Rule to that retreives the suit of a given card
Parameters:
   Input:
      Card: The card (represented by an integer)
   Output: 
      Rank: The suit of the card

Assistance Received: none
********************************************************************* */

get_card_suit(Card, Suit) :-
   num_of_unique_cards_in_game(NumOfUniqueCards),
   num_of_ranks_in_game(NumOfRanks),
   get_card_suit(Card, NumOfUniqueCards, NumOfRanks, Suit).


/* *********************************************************************
Rule Name: get_card_suit/3
Purpose: Rule that retreives the suit of a given card
Parameters:
   Input:
      Card: The card (represented by an integer)
      NumOfUniqueCards: the num of unique cards in the game 
   Output: 
      Suit: The suit of the card

Assistance Received: none
********************************************************************* */
get_card_suit(Card, NumOfUniqueCards, NumOfRanks, clubs) :-
   mod(Card, NumOfUniqueCards) < NumOfRanks.

get_card_suit(Card, NumOfUniqueCards, NumOfRanks, diamonds) :-
   mod(Card, NumOfUniqueCards) >= NumOfRanks,
   mod(Card, NumOfUniqueCards) < 2 * NumOfRanks.

get_card_suit(Card, NumOfUniqueCards, NumOfRanks, hearts) :-
   mod(Card, NumOfUniqueCards) >= 2 * NumOfRanks,
   mod(Card, NumOfUniqueCards) < 3 * NumOfRanks.

get_card_suit(Card, NumOfUniqueCards, NumOfRanks, spades) :-
   mod(Card, NumOfUniqueCards) >= 3 * NumOfRanks,
   mod(Card, NumOfUniqueCards) < 4 * NumOfRanks.



/* *********************************************************************
Rule Name: get_card_id_from_rank_and_suit/2
Purpose: Rule that generates the card id (int representation of a card) from given rank and suit.
            Since two cards with the same rank and suit can have different ids, this clause generates that 
            id that has the smaller value. 
Parameters:
   Input:
      Rank: The rank of the card
      Suit: The suit of the card
   Output: 
      CardId: The int representation of the card  
Assistance Received: none
********************************************************************* */
get_card_id_from_rank_and_suit(Rank, Suit, CardId) :-
   %get the rank and suit index based on the rank and strings
   rank_index(Rank, RankIndex),
   suit_index(Suit, SuitIndex),
   num_of_ranks_in_game(NumOfRanks),
   %the card id can be generated as shown in the expression below
   CardId is RankIndex + NumOfRanks * SuitIndex.


/* 
The index of each suit, needed to decide which rank beats which rank
 */

suit_index(clubs, 0).
suit_index(diamonds, 1).
suit_index(hearts, 2).
suit_index(spades, 3).


/* 
The string representation of each rank
 */
rank_string(nine, "9").
rank_string(jack, "J").
rank_string(queen, "Q").
rank_string(king, "K").
rank_string(ten, "X").
rank_string(ace, "A").


/*
   The string representation of each suit
*/
suit_string(clubs, "C").
suit_string(diamonds, "D").
suit_string(hearts, "H").
suit_string(spades, "S").


/*
   The points that each rank is worth
*/
rank_points(nine, 0).
rank_points(jack, 2).
rank_points(queen, 3).
rank_points(king, 4).
rank_points(ten, 10).
rank_points(ace, 11).

/* *********************************************************************
Rule Name: card_points/2
Purpose: Rule that generates the points a card is worth given its id
Parameters:
   Input:
      Card: The card (represented as an int) 
   Output: 
      CardPoints: The number of points the card is worth
Assistance Received: none
********************************************************************* */

card_points(Card, CardPoints) :-
   get_card_rank(Card, CardRank),
   rank_points(CardRank, CardPoints).


/* ************************************************************************************
Clauses related to cards used during serialization of game data
************************************************************************************ */


/* *********************************************************************
Rule Name: card_atom_type/2
Purpose: Rule that determines the atom type (asterisked or non-asterisked) of a card atom used for serialization
Parameters:
   Input:
      CardAtom: The card (represented as an atom that is used in serialization files)
   Output: 
      CardAtomType: The atom type of the card

Assistance Received: none
********************************************************************* */
card_atom_type(CardAtom, CardAtomType) :-
   %if the atom is comprised of only two characters, then it is a non-asterisked atom
   atom_chars(CardAtom, [_, _]),
   CardAtomType = non_asterisked.

card_atom_type(CardAtom, CardAtomType) :-
   %if the atom is comprised of more than two characters, then it is an asterisked atom
   atom_chars(CardAtom, [_, _, Asterisk]),
   Asterisk = '*',
   CardAtomType = asterisked.


/* *********************************************************************
Rule Name: card_id_to_atom/2
Purpose: Rule that generates the atom representation of a card (for use in serialization files),
               and generates the atom representing a suit if the a suit is sent as input argument instead of card
Parameters:
   Input:
      Card (or Suit): The card (represented as an int) or the suit
   Output: 
      CardAtom (or SuitAtom): The atom representation of the card (or suit)

Assistance Received: none
********************************************************************* */
card_id_to_atom(Suit, SuitAtom) :-
   %if the first argument is a suit and not a card, then convert the suit into its string representation
   suit_string(Suit, SuitString),

   %turn the representation into lowercase
   string_lower(SuitString, LowerCaseSuitString),

   %convert the lower-case string into an atom and add quotes around the atom
   atom_string(TempSuitAtom, LowerCaseSuitString),
   atom_concat(TempSuitAtom, "'", TempSuitAtom2),
   atom_concat("'", TempSuitAtom2, SuitAtom).

card_id_to_atom(Card, CardAtom) :-
   %convert the card into its string representation
   card_string(Card, CardString),

   %convert the string into lower case
   string_lower(CardString, LowerCaseCardString),

   %convert the string into an atom and add quotes around it
   atom_string(TempCardAtom, LowerCaseCardString),
   atom_concat(TempCardAtom, "'", TempCardAtom2),
   atom_concat("'", TempCardAtom2, CardAtom).


/* *********************************************************************
Rule Name: card_id_to_asterisked_atom/2
Purpose: Rule that generates the atom representation of a card with an asterisk at the end (for use for melds in serialization files)
Parameters:
   Input:
      Card: The card (represented as an int) or the suit
   Output: 
      CardAtom (or SuitAtom): The atom representation of the card (or suit) with an asterisk at the end
Assistance Received: none
********************************************************************* */
card_id_to_asterisked_atom(Card, CardAtom) :-
   %convert the card into its string representation
   card_string(Card, CardString),

   %change the string to lower case and add an asterisk to the end of the string
   string_lower(CardString, LowerCaseCardString),
   string_concat(LowerCaseCardString, "*", AsteriskedCardString),

   %convert the string to its atomic representation and add quotes on each side
   atom_string(TempCardAtom, AsteriskedCardString),
   atom_concat(TempCardAtom, "'", TempCardAtom2),
   atom_concat("'", TempCardAtom2, CardAtom).


/* *********************************************************************
Rule Name: card_atom_to_id/2
Purpose: Rule that generates the card id (int representation of a card) from its atomic representation.
            Since two cards with the same rank and suit can have different ids, this clause generates that 
            id that has the smaller value. Also if an atomic representation of a suit used for serialization files instead of that of a card is given,
            then it generates the corresponding atom representing the suit
Parameters:
   Input:
      CardAtom: The card (represented as an atom) 
   Output: 
      CardId: The int representation of the card 
Assistance Received: none
********************************************************************* */
card_atom_to_id(CardAtom, CardId):-
   %separate out the characters of the card into individual atoms 
   %if the clause fails, it must mean that there are more than two characters in the atom
   %so move on to the next clause that converts card atoms with asterisks into int representation of cards
   atom_chars(CardAtom, [RankAtom, SuitAtom]),

   %convert the separated atoms to their string representation and convert to uppercase
   atom_string(RankAtom, LCaseRankString),
   atom_string(SuitAtom, LCaseSuitString),
   string_upper(LCaseRankString, RankString),
   string_upper(LCaseSuitString, SuitString),

   %use the strings to determine the rank and suit of the cards 
   rank_string(Rank, RankString),
   suit_string(Suit, SuitString),
   
   %get the rank and suit index based on the rank and strings
   rank_index(Rank, RankIndex),
   suit_index(Suit, SuitIndex),
   num_of_ranks_in_game(NumOfRanks),

   %the card id can be generated as shown in the expression below
   CardId is RankIndex + NumOfRanks * SuitIndex.


card_atom_to_id(CardAtom, CardId):-
   %separate out the characters of the card into individual atoms 
   %if the clause fails, it must mean that there are more or less than three characters in the atom
   atom_chars(CardAtom, [RankAtom, SuitAtom, Asterisk]),

   %if the third character in the atom is an asterisk, then it is an asterisked card 
   Asterisk = '*',

   %convert the separated atoms to their string representation and convert to uppercase
   atom_string(RankAtom, LCaseRankString),
   atom_string(SuitAtom, LCaseSuitString),
   string_upper(LCaseRankString, RankString),
   string_upper(LCaseSuitString, SuitString),

   %use the strings to determine the rank and suit of the cards 
   rank_string(Rank, RankString),
   suit_string(Suit, SuitString),
   
   %get the rank and suit index based on the rank and strings
   rank_index(Rank, RankIndex),
   suit_index(Suit, SuitIndex),
   num_of_ranks_in_game(NumOfRanks),

   %the card id can be generated as shown in the expression below
   CardId is RankIndex + NumOfRanks * SuitIndex.

card_atom_to_id(SuitAtom, Suit):-
   %if the previous two clauses fail, it must mean that the atom represents a suit and not a card

   %conver the atom into its string representation
   atom_string(SuitAtom, LCaseSuitString),
   string_upper(LCaseSuitString, LongSuitString),
   sub_string(LongSuitString, 0, 1, _, SuitString),
   suit_string(Suit, SuitString).

 

/* *********************************************************************
Rule Name: card_string/2
Purpose: Rule that generates the string representation of a card given its id
Parameters:
   Input:
      CardAtom: The card (represented as an atom) 
   Output: 
      CardId: The int representation of the card 
Assistance Received: none
********************************************************************* */
card_string(Card, CardString) :-
   get_card_rank(Card, Rank),
   get_card_suit(Card, Suit),
   rank_string(Rank, RankString),
   suit_string(Suit, SuitString),
   string_concat(RankString, SuitString, CardString).




/* ************************************************************************************
Clauses related to comparison of cards
************************************************************************************ */



/* *********************************************************************
Rule Name: card_or_suit_string/2
Purpose: Rule that generates the string representation of a card given its id 
            or the string representation of a suit given a suit atom
Parameters:
   Input:
      CardOrSuit: The card id or suit
   Output: 
      CardOrSuitString: The string representation of the card or suit
Assistance Received: none
********************************************************************* */
card_or_suit_string(CardOrSuit, CardOrSuitString) :-
   suit_string(CardOrSuit, CardOrSuitString).

card_or_suit_string(CardOrSuit, CardOrSuitString) :-
   card_string(CardOrSuit, CardOrSuitString).



/* *********************************************************************
Rule Name: card_with_higher_rank/2
Purpose: Returns that card which has the higher rank between two cards
Parameters:
   Input:
      Card1: The first card to be compared
      Card2: The second card to be compared
   Output: 
      Card: The card with the greater rank. If they have the same rank,
               return the atom same_rank
Assistance Received: none
********************************************************************* */
card_with_higher_rank(Card1, Card2, Card1) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, Rank1).

card_with_higher_rank(Card1, Card2, Card2) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, Rank2).

card_with_higher_rank(Card1, Card2, same_rank) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, same_rank).


/* *********************************************************************
Rule Name: card_with_less_rank/2
Purpose: Returns that card which has the less rank between two cards
Parameters:
   Input:
      Card1: The first card to be compared
      Card2: The second card to be compared
   Output: 
      Card: The card with less rank. If they have the same rank,
               return the atom same_rank
Assistance Received: none
********************************************************************* */

card_with_less_rank(Card1, Card2, Card1) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, Rank2).

card_with_less_rank(Card1, Card2, Card2) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, Rank1).

card_with_less_rank(Card1, Card2, same_rank) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, same_rank).



/* *********************************************************************
Rule Name: card_with_same_or_less_rank/2
Purpose: Returns that card which has the less rank between two cards.
            Returns the second card if both cards have the same rank
Parameters:
   Input:
      Card1: The first card to be compared
      Card2: The second card to be compared
   Output: 
      Card: The card with less rank. If they have the same rank,
               return the second card
Assistance Received: none
********************************************************************* */

card_with_same_or_less_rank(Card1, Card2, Card1) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   compare_ranks(Rank1, Rank2, Rank2).

card_with_same_or_less_rank(Card1, Card2, Card2) :-
   get_card_rank(Card1, Rank1),
   get_card_rank(Card2, Rank2),
   \+ (compare_ranks(Rank1, Rank2, Rank2)).



/* *********************************************************************
Rule Name: compare_ranks/2
Purpose: Returns that rank which is of greater value. Returns the atom same_rank
         if they have the same rank
Parameters:
   Input:
      Rank1: The first rank to be compared
      Rank2: The second rank to be compared
   Output: 
      Rank: The rank with greater value. If they have the same value,
               return the atom same_rank
Assistance Received: none
********************************************************************* */

compare_ranks(Rank, Rank, same_rank).

compare_ranks(Rank1, Rank2, GreaterRank) :-
   rank_index(Rank1, Rank1Index),
   rank_index(Rank2, Rank2Index),
   compare_ranks_by_index(Rank1Index, Rank2Index, GreaterRankIndex),
   rank_index(GreaterRank, GreaterRankIndex).


/* *********************************************************************
Rule Name: compare_ranks_by_index/2
Purpose: Returns that rank index which is of greater value. Returns -1 if they have the same index
Parameters:
   Input:
      Rank1Index: The first rank index to be compared
      Rank2Index: The second rank index to be compared
   Output: 
      RankIndex: The rank index with greater value. If they have the same value,
               return -1
Assistance Received: none
********************************************************************* */
compare_ranks_by_index(Rank1Index, Rank2Index, Rank1Index) :-
   Rank1Index > Rank2Index.

compare_ranks_by_index(Rank1Index, Rank2Index, Rank2Index) :-
   Rank1Index < Rank2Index.

compare_ranks_by_index(RankIndex, RankIndex, -1).



/* *********************************************************************
Rule Name: card_with_higher_suit/2
Purpose: Returns that card which has the higher suit between two cards
Parameters:
   Input:
      Card1: The first card to be compared
      Card2: The second card to be compared
   Output: 
      Card: The card with the "greater" suit (meaning the suit that comes later in the list of suits). If they have the same suit,
               return the atom same_suit
Assistance Received: none
********************************************************************* */
card_with_higher_suit(Card1, Card2, Card1) :-
   get_card_suit(Card1, Suit1),
   get_card_suit(Card2, Suit2),
   compare_suits(Suit1, Suit2, Suit1).

card_with_higher_suit(Card1, Card2, Card2) :-
   get_card_suit(Card1, Suit1),
   get_card_suit(Card2, Suit2),
   compare_suits(Suit1, Suit2, Suit2).

card_with_higher_suit(Card1, Card2, same_suit) :-
   get_card_suit(Card1, Suit1),
   get_card_suit(Card2, Suit2),
   compare_suits(Suit1, Suit2, same_suit).



/* *********************************************************************
Rule Name: card_with_same_or_less_suit/2
Purpose: Returns that card which has the less suit between two cards.
            Returns the second card if both cards have the same suit
Parameters:
   Input:
      Card1: The first card to be compared
      Card2: The second card to be compared
   Output: 
      Card: The card with "less" suit (meaning the suit that comes first in the list of suits). If they have the same suit,
               return the second card
Assistance Received: none
********************************************************************* */
card_with_same_or_less_suit(Card1, Card2, Card1) :-
   get_card_suit(Card1, Suit1),
   get_card_suit(Card2, Suit2),
   compare_suits(Suit1, Suit2, Suit2).

card_with_same_or_less_suit(Card1, Card2, Card2) :-
   get_card_suit(Card1, Suit1),
   get_card_suit(Card2, Suit2),
   \+ (compare_suits(Suit1, Suit2, Suit2)).



/* *********************************************************************
Rule Name: compare_suits/2
Purpose: Returns that suit which is of greater value. Returns the atom same_suit
         if they have the same suit
Parameters:
   Input:
      Suit1: The first suit to be compared
      Suit2: The second suit to be compared
   Output: 
      Suit: The suit with "greater" value (the suit that comes later in the list of suits). If they have the same value,
               return the atom same_suit
Assistance Received: none
********************************************************************* */
compare_suits(Suit, Suit, same_suit).

compare_suits(Suit1, Suit2, GreaterSuit) :-
   suit_index(Suit1, Suit1Index),
   suit_index(Suit2, Suit2Index),
   compare_suits_by_index(Suit1Index, Suit2Index, GreaterSuitIndex),
   suit_index(GreaterSuit, GreaterSuitIndex).



/* *********************************************************************
Rule Name: compare_suits_by_index/2
Purpose: Returns that suit index which is of greater value. Returns -1 if they have the same index
Parameters:
   Input:
      Suit1Index: The first suit index to be compared
      Suit2Index: The second suit index to be compared
   Output: 
      SuitIndex: The suit index with greater value. If they have the same value,
               return -1
Assistance Received: none
********************************************************************* */
compare_suits_by_index(Suit1Index, Suit2Index, Suit1Index) :-
   Suit1Index > Suit2Index.

compare_suits_by_index(Suit1Index, Suit2Index, Suit2Index) :-
   Suit1Index < Suit2Index.

compare_suits_by_index(SuitIndex, SuitIndex, -1).