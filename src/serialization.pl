:- module(serialization, [serialize/15, deserialize/15]).

:- use_module(cards).
:- use_module(card_pile).
:- use_module(melds).



/* ********************************************************************************************************
Clauses that serialize the game state
******************************************************************************************************** */

/* *********************************************************************
Rule Name: serialize/15
Purpose: Generates serialized data from the current game state
Parameters:
   Input:
      CmptrGameScr: the computer game score
      HmnGameScr: the human game score
      CmptrRndScore: the computer round score
      HmnRndScore: the human round score
      RoundNumber: the round number
      Stock: the stock
      TrmpCrd: the trump card
      NextPlayer: the next player
      CmptrHnd: the computer hand
      HmnHnd: the human hand
      CmptrCptrPile: the computer capture pile
      HmnCptrPile: the human capture pile
      CmptrMelds: the melds played by the computer
      HmnMelds: the melds played by the human
   Output:
      SerializedData: The serialized data
Assistance Received: none
********************************************************************* */

serialize(CmptrGameScr, 
            HmnGameScr, 
            CmptrRndScore,
            HmnRndScore,
            RoundNumber, 
            Stock, 
            TrmpCrd,
            NextPlayer,
            CmptrHnd, 
            HmnHnd, 
            CmptrCptrPile, 
            HmnCptrPile,
            CmptrMelds,
            HmnMelds, 
            SerializedData) :-
   serialize_player_cards(HmnHnd, HmnCptrPile, HmnMelds, SerializedHmnHnd, SerializedHmnCptrPile, SerializedHmnMelds),
   serialize_player_cards(CmptrHnd, CmptrCptrPile, CmptrMelds, SerializedCmptrHnd, SerializedCmptrCptrPile, SerializedCmptrMelds),

   card_id_to_atom(TrmpCrd, SerializedTrmpCrd),
   serialize_list_of_cards(Stock, SerializedStock),

   SerializedData = [RoundNumber, 
                     [CmptrGameScr, CmptrRndScore], 
                     SerializedCmptrHnd, 
                     SerializedCmptrCptrPile, 
                     SerializedCmptrMelds, 
                     [HmnGameScr, HmnRndScore], 
                     SerializedHmnHnd, 
                     SerializedHmnCptrPile, 
                     SerializedHmnMelds,
                     SerializedTrmpCrd,
                     SerializedStock,
                     NextPlayer
                     ].


/* *********************************************************************
Rule Name: serialize_player_cards/6
Purpose: Generates serialized data for a player's cards
Parameters:
   Input:
      Hand: The player's hand
      CapturePile: The player's capture pile
      MeldsPlayed: The melds played by the player
   Output:
      SerializedHand: The serialized hand 
      SerializedCapturePile: The serialized Capture Pile 
      SerializedMelds: The serialized Melds 
   Algorithm: 
      1) for each card in hand:
      2)    for each meld instance that the card is part of
      3)         if the meld instance is a "complete meld instance", as defined below, then 
      4)               check if the meld instance has already been encountered before (through another card in hand)
      5)                   if not encountered before, then store the meld instance
      6)                   if encountered before, then do not store the meld instance
      7)               increment the count for the number of meld instances that the card is part of
      8) create a hand serialized representation from the hand, comprising of only those cards that have meld instance count of 0
      9) create a capture string from the capturePile
      10) create a meld serialized representation of all the cards in hand with more than 0 meld instances,
               and if the card has more than 1 meld instances, add an asterick next to it 
Assistance Received: none
********************************************************************* */   
serialize_player_cards(Hand, CapturePile, MeldsPlayed, SerializedHand, SerializedCapturePile, SerializedMelds) :-
   %for each card in hand, we count how many "complete" meld instances it is part of 
   %By "complete" meld instances, we mean those meld instances whose component cards are all still in hand
   %if any card of a previously played instance has already been thrown, it is not a complete meld instance

   %if a card has 0 such meld instances, the card goes to hand string
   %if a card has 1 such meld instances, the card goes to meld string
   %if a card has more than 1 such meld instances, the card goes to meld string marked by an asterisk(*)

   extract_complete_melds_from_hand(Hand, MeldsPlayed, CompleteMelds, CompleteMeldsCounts),

   get_serialized_hand(Hand, CompleteMeldsCounts, SerializedHand),

   serialize_list_of_cards(CapturePile, SerializedCapturePile),

   get_serialized_melds(Hand, CompleteMelds, CompleteMeldsCounts, SerializedMelds).



/* *********************************************************************
Rule Name: get_serialized_hand/3
Purpose: Serializes the given hand
Parameters:
   Input:
      Hand: The player's hand
      CompleteMeldsCounts: The list containing the number of complete melds each card in the hand is part of
   Output:
      SerializedHand: The serialized hand 
   Algorithm:
      1) if a card in the hand is not a part of any complete meld, then convert it to its serialized form and make it part of the serialized hand
      2) Do the same for the rest of the cards
Assistance Received: none
********************************************************************* */   
get_serialized_hand([], _, []).

get_serialized_hand([FirstCard | RestOfHand], [FirstCardMeldCounts | RestOfHandMeldCounts], SerializedHand) :-
   FirstCardMeldCounts = 0,
   get_serialized_hand(RestOfHand, RestOfHandMeldCounts, ProvisionalSerializedHand),
   card_id_to_atom(FirstCard, CardAtom),
   SerializedHand = [CardAtom | ProvisionalSerializedHand].

get_serialized_hand([_ | RestOfHand], [FirstCardMeldCounts | RestOfHandMeldCounts], SerializedHand) :-
   FirstCardMeldCounts \= 0,
   get_serialized_hand(RestOfHand, RestOfHandMeldCounts, SerializedHand).


/* *********************************************************************
Rule Name: serialize_list_of_cards/2
Purpose: Serializes a list of cards
Parameters:
   Input:
      Cards: The cards that need to be serialized
   Output:
      SerializedCards: The cards in serialized form
Assistance Received: none
********************************************************************* */   
serialize_list_of_cards([], []).

serialize_list_of_cards([FirstCard | RemainingCards], SerializedCards) :-
   serialize_list_of_cards(RemainingCards, IncompleteSerializedCards),
   card_id_to_atom(FirstCard, CardAtom),
   SerializedCards = [CardAtom | IncompleteSerializedCards].



/* *********************************************************************
Rule Name: get_serialized_melds/2
Purpose: Generates the serialized version of the melds played by a player. 
Parameters:
   Input:
      Hand: the player's hand
      CompleteMelds: The list of all complete melds
      CompleteMeldsCounts: The list containing the number of complete melds each card in the hand is part of
   Output:
      SerializedMelds: The melds in serialized form
      
Assistance Received: none
********************************************************************* */   
get_serialized_melds(_, [], _, []).

get_serialized_melds(Hand, [FirstCompleteMeld | RemainingCompleteMelds], CompleteMeldsCounts, SerializedMelds) :-
   get_serialized_melds(Hand, RemainingCompleteMelds, CompleteMeldsCounts, RemainingSerializedMelds),
   serialize_meld(Hand, FirstCompleteMeld, CompleteMeldsCounts, SerializedMeld),
   SerializedMelds = [SerializedMeld | RemainingSerializedMelds].



/* *********************************************************************
Rule Name: serialize_meld/4
Purpose: Serializes a given meld
Parameters:
   Input:
      Hand: the player's hand
      MeldCards: The cards comprising a meld
      CompleteMeldsCounts: The list containing the number of complete melds each card in the hand is part of
   Output:
      SerializedMeld: The meld in serialized form
Algorithm:
      1) If the number of complete melds that a card in the MeldCards pile is two or more, then serialize that card as an asterisked meld card
      2) If the number of complete melds that a card in the MeldCards pile is less than two, then serialize that card as a non-asterisked meld card
      3) add all the serialized cards to a list and return that as that serialized meld

Assistance Received: none
********************************************************************* */ 
serialize_meld(_, [], _, []).

serialize_meld(Hand, [FirstCard | RemainingCards], CompleteMeldsCounts, SerializedMeld) :-
   %determining how many complete melds is this card a part of
   get_card_position_in_pile(Hand, FirstCard, CardPosition),
   get_item_by_position(CompleteMeldsCounts, CardPosition, NumOfCompleteMelds),
   NumOfCompleteMelds > 1,
   serialize_meld(Hand, RemainingCards, CompleteMeldsCounts, PartiallySerializedMeld),
   card_id_to_asterisked_atom(FirstCard, CardAtom),
   SerializedMeld = [CardAtom | PartiallySerializedMeld].

serialize_meld(Hand, [FirstCard | RemainingCards], CompleteMeldsCounts, SerializedMeld) :-
   %determining how many complete melds is this card a part of
   get_card_position_in_pile(Hand, FirstCard, CardPosition),
   get_item_by_position(CompleteMeldsCounts, CardPosition, NumOfCompleteMelds),
   NumOfCompleteMelds =< 1,
   serialize_meld(Hand, RemainingCards, CompleteMeldsCounts, PartiallySerializedMeld),
   card_id_to_atom(FirstCard, CardAtom),
   SerializedMeld = [CardAtom | PartiallySerializedMeld].


/* *********************************************************************
Rule Name: extract_complete_melds_from_hand/4
Purpose: Extracts the "compelete melds" from a hand. A complete meld is a meld that has all of its component cards still in hand, i.e. not 
            of the cards used to create the meld have yet been thrown by the player
Parameters:
   Input:
      Hand: the player's hand
      MeldsPlayed: THe melds played by the player
   Output:
      CompleteMelds: The list of all the complete melds in hand
      CompleteMeldsCounts: The list containing the number of complete melds each card in the hand is part of
Algorithm:
      1) For each card in hand:
      2)        Get all the list of all the melds that a card is part
      3)        Retreive only the complete melds from that list and add to the list of complete melds
      4)        Add the number of complete melds from that list to the complete melds counts list

Assistance Received: none
********************************************************************* */ 
extract_complete_melds_from_hand(Hand, MeldsPlayed, CompleteMelds, CompleteMeldsCounts) :-
   extract_complete_melds_from_hand(Hand, 0, MeldsPlayed, [], CompleteMelds, CompleteMeldsCounts).

extract_complete_melds_from_hand(Hand, CurrentHandPosition, _, CompleteMelds, CompleteMelds, []):-
   length(Hand, NumOfCardsInHand),
   CurrentHandPosition >= NumOfCardsInHand.

extract_complete_melds_from_hand(Hand, CurrentHandPosition, MeldsPlayed, CompleteMelds, UpdatedCompleteMelds, CompleteMeldsCounts) :-
   NewHandPosition is CurrentHandPosition + 1,
   extract_complete_melds_from_hand(Hand, NewHandPosition, MeldsPlayed, CompleteMelds, NewCompleteMelds, NewCompleteMeldsCounts),
   get_card_by_position(Hand, CurrentHandPosition, Card),
   get_all_melds_containing_card(MeldsPlayed, Card, AllMeldsUsingCard),
   retrieve_complete_melds_from_melds_list(Hand, AllMeldsUsingCard, NewCompleteMelds, UpdatedCompleteMelds, NumOfCompleteMeldsFound),
   CompleteMeldsCounts = [NumOfCompleteMeldsFound | NewCompleteMeldsCounts].


/* *********************************************************************
Rule Name: retrieve_complete_melds_from_melds_list/5
Purpose: Extracts the "compelete melds" from a list of melds
Parameters:
   Input:
      Hand: the player's hand
      Melds: THe list of melds from which complete melds are to be extracted
      CompleteMelds: The previous list of complete melds to which the newly extracted complete melds will be added
   Output:
      UpdatedCompleteMelds: The updated list of all the complete melds in hand
      CompleteMeldsCounts: The of complete melds in the given list of melds 
Algorithm:
      1) For each meld in the list of melds:
      2)        If the player's hand contains all the cards in the meld and if the meld has not been encountered before
      3)             then add meld to the list of completed melds and increment complete melds count 
      3)        Else move on to the next card
      4) Return the list of completed melds and the complete melds count

Assistance Received: none
********************************************************************* */ 
retrieve_complete_melds_from_melds_list(Hand,Melds,CompleteMelds,UpdatedCompleteMelds,CompleteMeldsCount) :-
   retrieve_complete_melds_from_melds_list(Hand,Melds,CompleteMelds, 0, UpdatedCompleteMelds,CompleteMeldsCount).

retrieve_complete_melds_from_melds_list(_, [], CompleteMelds, CompleteMeldsCount, CompleteMelds, CompleteMeldsCount).

retrieve_complete_melds_from_melds_list(
                     Hand,
                     [FirstMeld|RemainingMelds],
                     CompleteMelds,
                     CompleteMeldsCount,
                     UpdatedCompleteMelds,
                     UpdatedCompleteMeldsCount) :-
   %check if the meld is NOT a complete meld (i.e. that all the cards comprising the meld are not still in hand)
   \+ (card_list_contains_all_given_cards(Hand, FirstMeld)),
   retrieve_complete_melds_from_melds_list(Hand, RemainingMelds, CompleteMelds, CompleteMeldsCount, UpdatedCompleteMelds, UpdatedCompleteMeldsCount).

retrieve_complete_melds_from_melds_list(
                     Hand,
                     [FirstMeld|RemainingMelds],
                     CompleteMelds,
                     CompleteMeldsCount,
                     UpdatedCompleteMelds,
                     UpdatedCompleteMeldsCount) :-

   %check if the meld is a complete meld (i.e. that all the cards comprising the meld are still in hand)
   card_list_contains_all_given_cards(Hand, FirstMeld),

   \+ (meld_has_already_been_encountered(FirstMeld, CompleteMelds)),
   NewCompleteMeldsCounts is CompleteMeldsCount + 1,
   NewCompleteMelds = [FirstMeld | CompleteMelds],
   retrieve_complete_melds_from_melds_list(Hand,  RemainingMelds, NewCompleteMelds, NewCompleteMeldsCounts, UpdatedCompleteMelds, UpdatedCompleteMeldsCount).

   
retrieve_complete_melds_from_melds_list(
                     Hand,
                     [FirstMeld|RemainingMelds],
                     CompleteMelds,
                     CompleteMeldsCount,
                     UpdatedCompleteMelds,
                     UpdatedCompleteMeldsCount) :-

   %check if the meld is a complete meld (i.e. that all the cards comprising the meld are still in hand)
   card_list_contains_all_given_cards(Hand, FirstMeld),
   meld_has_already_been_encountered(FirstMeld, CompleteMelds),
   NewCompleteMeldsCount is CompleteMeldsCount + 1,
   retrieve_complete_melds_from_melds_list(Hand,  RemainingMelds, CompleteMelds, NewCompleteMeldsCount, UpdatedCompleteMelds, UpdatedCompleteMeldsCount).



/* *********************************************************************
Rule Name: meld_has_already_been_encountered/5
Purpose: Succeeds only if the melds has already been encountered 
Parameters:
   Input:
      Meld: the meld that is being checked if it has already been encountered
      Melds: THe list of complete melds
   Output:
Assistance Received: none
********************************************************************* */ 
meld_has_already_been_encountered(_, []) :-
   false.

meld_has_already_been_encountered(Meld, [FirstCompleteMeld | _]) :-
   Meld = FirstCompleteMeld.

meld_has_already_been_encountered(Meld, [FirstCompleteMeld | RemainingCompleteMelds]) :-
   Meld \= FirstCompleteMeld, 
   meld_has_already_been_encountered(Meld, RemainingCompleteMelds).


/* ********************************************************************************************************
Clauses that deserialize the game state
******************************************************************************************************** */


/* *********************************************************************
Rule Name: deserialize/15
Purpose: Deserialize the data in a save file
Parameters:
   Input:
      SaveFileData: The save file data to be deserialized
   Output:
      CmptrGameScr: the deserialized computer game score
      HmnGameScr: the deserialized human game score
      CmptrRndScore: the deserialized computer round score
      HmnRndScore: the deserialized human round score
      RoundNumber: the deserialized round number
      Stock: the deserialized stock
      TrmpCrd: the deserialized trump card
      NextPlayer: the deserialized next player
      CmptrHnd: the deserialized computer hand
      HmnHnd: the deserialized human hand
      CmptrCptrPile: the deserialized computer capture pile
      HmnCptrPile: the deserialized human capture pile
      CmptrMelds: the deserialized melds played by the computer
      HmnMelds: the deserialized melds played by the human
Assistance Received: none
********************************************************************* */ 
deserialize(SaveFileData, 
            CmptrGameScr, 
            HmnGameScr, 
            CmptrRndScore,
            HmnRndScore,
            RoundNumber, 
            Stock, 
            TrmpCrd,
            NextPlayer,
            CmptrHnd, 
            HmnHnd, 
            CmptrCptrPile, 
            HmnCptrPile,
            CmptrMelds,
            HmnMelds) :-

   [
      RoundNumber, 
      [CmptrGameScr, CmptrRndScore], 
      SerializedCmptrHnd, 
      SerializedCmptrCptrPile, 
      SerializedCmptrMelds, 
      [HmnGameScr, HmnRndScore], 
      SerializedHmnHnd, 
      SerializedHmnCptrPile, 
      SerializedHmnMelds,
      SerializedTrmpCrd,
      SerializedStock,
      NextPlayer
      ] = SaveFileData,

   deserialize_list_of_cards(SerializedStock, [], Stock),
   CardsEncountered = Stock,
   deserialize_card(SerializedTrmpCrd, CardsEncountered, TrmpCrd),
   UpdatedCardsEncountered = [TrmpCrd | CardsEncountered],

   get_suit_from_trump_card(TrmpCrd, TrumpSuit),
   
   deserialize_player_cards(SerializedHmnHnd, SerializedHmnCptrPile, SerializedHmnMelds, TrumpSuit, UpdatedCardsEncountered, HmnHnd, HmnCptrPile, HmnMelds, UpdatedCardsEncountered2),
   deserialize_player_cards(SerializedCmptrHnd, SerializedCmptrCptrPile, SerializedCmptrMelds, TrumpSuit, UpdatedCardsEncountered2, CmptrHnd, CmptrCptrPile, CmptrMelds, _).



/* *********************************************************************
Rule Name: deserialize_player_cards/9
Purpose: Deserialize a player's hand, capture pile and melds
Parameters:
   Input:
      SerializedHand: The serialized hand
      SerializedCapturePile: the serialized capture pile
      SerializedMelds: The serialized melds
      TrumpSuit: The serialized trump suit
      CardsEncountered: The list of cards encountered so far
   Output:
      Hand: The deserialized hand
      CapturePile: The deserialized capture pile
      MeldsPlayed: The deserialized melds played
      UpdatedCardsEncountered: The updated cards encountered
Assistance Received: none
********************************************************************* */ 
deserialize_player_cards(SerializedHand, SerializedCapturePile, SerializedMelds, TrumpSuit, CardsEncountered, Hand, CapturePile, MeldsPlayed, UpdatedCardsEncountered) :-
   %deserialize the serialized list of hands 
   %note that the hand is still not complete since cards for the hand still have to be extracted from the serialized meld
   deserialize_list_of_cards(SerializedHand, CardsEncountered, IncompleteHand),
   append(IncompleteHand, CardsEncountered, NewCardsEncountered),

   %deserialize the capture pile
   deserialize_list_of_cards(SerializedCapturePile, NewCardsEncountered, CapturePile),
   append(CapturePile, NewCardsEncountered, NewCardsEncountered2),

   %deserialize the melds (and the rest of the hand)
   deserialize_melds(SerializedMelds, IncompleteHand, TrumpSuit, NewCardsEncountered2, MeldsPlayed, Hand, UpdatedCardsEncountered).



/* *********************************************************************
Rule Name: deserialize_melds/7
Purpose: Deserialize a player's hand, capture pile and melds
Parameters:
   Input:
      SerializedMelds: The serialized melds
      Hand: The incomplete deserialized hand
      TrumpSuit: the trump suit for the round
      CardsEncountered: The cards encountered so far
   Output:
      MeldsPlayed: The deserialized melds played
      UpdatedHand: The complete deserialized hand
      UpdatedCardsEncountered: The updated list of cards encountered
   Algorithm:
         1) For each meld in the serialized melds list
         2)    In the first meld in the list of serialized melds, separate out the asterisked and non-asterisked cards
         3)    deserialize and extract only the previously unencountered *asterisked* cards from the list of asterisked cards and add those to list of encountered cards and to hand
         4)    Deserialize and extract all the non-asterisked cards and add those to the list of encountered cards and to hand
         5)    Add *all* the deserialized asterisked cards (even the ones previously encountered) and the non-asterisked cards to form a meld
         6)    Add the formed meld to the deserialized melds storage
Assistance Received: none
********************************************************************* */ 
deserialize_melds(SerializedMelds, Hand, TrumpSuit, CardsEncountered, MeldsPlayed, UpdatedHand, UpdatedCardsEncountered) :-
   deserialize_melds(SerializedMelds, Hand, TrumpSuit, CardsEncountered, MeldsPlayed, UpdatedHand, UpdatedCardsEncountered, _).


deserialize_melds([], Hand, _, CardsEncountered, MeldsPlayed, Hand, CardsEncountered, []):-
   generate_empty_melds_storage(MeldsPlayed).


deserialize_melds(
   [FirstSerializedMeld | RemainingSerializedMelds], 
   Hand,
   TrumpSuit,
   CardsEncountered, 
   MeldsPlayed, 
   UpdatedHand,
   UpdatedCardsEncountered,
   EncounteredAsteriskedCards
   ) :-

   deserialize_melds(RemainingSerializedMelds, Hand, TrumpSuit, CardsEncountered, IncompleteMeldsPlayed, NewHand, NewCardsEncountered, NewEncounteredAsteriskedCards),
   
   separate_asterisked_and_non_asterisked_cards(FirstSerializedMeld, AsteriskedCardsAtoms, NonAsteriskedCardsAtoms),

   extract_unencountered_asterisked_cards(
      AsteriskedCardsAtoms, 
      NewHand, 
      NewCardsEncountered,
      NewEncounteredAsteriskedCards, 
      NewCardsEncountered2, 
      EncounteredAsteriskedCards, 
      PartialFirstMeld, 
      NewHand2),
   
   
   extract_non_asterisked_cards(NonAsteriskedCardsAtoms, NewHand2, NewCardsEncountered2, PartialFirstMeld, UpdatedHand, UpdatedCardsEncountered, CompletedFirstMeld),
   create_meld(CompletedFirstMeld, TrumpSuit, MeldType),
   add_meld_to_storage(IncompleteMeldsPlayed, CompletedFirstMeld, MeldType, MeldsPlayed).

/* *********************************************************************
Rule Name: extract_unencountered_asterisked_cards/8
Purpose: Deserialize and extract all the asterisked cards that have not been previously encountered from a list of serialized asteriske cards
Parameters:
   Input:
      AsteriskedCards: The list of all serialized asterisked cards
      Hand: The incomplete deserialized hand
      CardsEncountered: The cards encountered so far
      EncounteredAsteriskedCards: The asterisked cards encountered so far 
   Output:
      UpdatedCardsEncountered: The updated list of cards encountered
      UpdatedEncounteredAsteriskedCards: The updated list of asterisked cards encountered
      Meld: The unfinished meld (with only asterisked cards present)
      UpdatedHand: The updated hand
   Algorithm:
         1) For each asterisked serialized cards in the list of asterisked serialized cards
         2)    If the card is in the list of encountered asterisked cards, then simply add the card to the unfinished meld
         3)    If the card is not the list of encountered asterisked cards, then 
         4)          Add the card to the list of encountered asterisked cards
         5)          Add the card to the unfinished meld
         6)          Add the card to he list of encountered cards
         7)          Add the card to the hand
Assistance Received: none
********************************************************************* */ 
extract_unencountered_asterisked_cards(
                              AsteriskedCardsAtoms, 
                              Hand,
                              CardsEncountered, 
                              EncounteredAsteriskedCards,
                              UpdatedCardsEncountered, 
                              UpdatedEncounteredAsteriskedCards, 
                              Meld,
                              UpdatedHand) :-
   extract_unencountered_asterisked_cards(
                                 AsteriskedCardsAtoms, 
                                 Hand,
                                 CardsEncountered, 
                                 EncounteredAsteriskedCards,
                                 [], 
                                 UpdatedCardsEncountered, 
                                 UpdatedEncounteredAsteriskedCards, 
                                 Meld,
                                 UpdatedHand).

extract_unencountered_asterisked_cards(
                              [], 
                              Hand,
                              CardsEncountered, 
                              EncounteredAsteriskedCards,
                              IncompleteMeld, 
                              UpdatedCardsEncountered, 
                              UpdatedEncounteredAsteriskedCards, 
                              Meld,
                              UpdatedHand) :-
   UpdatedHand = Hand,
   Meld = IncompleteMeld,
   UpdatedEncounteredAsteriskedCards = EncounteredAsteriskedCards,
   UpdatedCardsEncountered = CardsEncountered.


extract_unencountered_asterisked_cards(
                              [FirstAsteriskedCardAtom | RemainingCardAtoms], 
                              Hand,
                              CardsEncountered, 
                              EncounteredAsteriskedCards,
                              IncompleteMeld, 
                              UpdatedCardsEncountered, 
                              UpdatedEncounteredAsteriskedCards, 
                              Meld,
                              UpdatedHand) :-
   card_atom_to_id(FirstAsteriskedCardAtom, TempCardId),
   get_card_rank(TempCardId, CardRank),
   get_card_suit(TempCardId, CardSuit),
   get_cards_by_rank_and_suit(EncounteredAsteriskedCards, CardRank, CardSuit, [Card | _]),
   NewIncompleteMeld = [Card | IncompleteMeld],
   extract_unencountered_asterisked_cards(
                              RemainingCardAtoms, 
                              Hand, 
                              CardsEncountered,
                              EncounteredAsteriskedCards, 
                              NewIncompleteMeld,
                              UpdatedCardsEncountered,
                              UpdatedEncounteredAsteriskedCards,
                              Meld,
                              UpdatedHand
                              ).
   
extract_unencountered_asterisked_cards(
                              [FirstAsteriskedCardAtom | RemainingCardAtoms], 
                              Hand,
                              CardsEncountered, 
                              EncounteredAsteriskedCards,
                              IncompleteMeld, 
                              UpdatedCardsEncountered, 
                              UpdatedEncounteredAsteriskedCards, 
                              Meld,
                              UpdatedHand) :-
   card_atom_to_id(FirstAsteriskedCardAtom, TempCardId),
   get_card_rank(TempCardId, CardRank),
   get_card_suit(TempCardId, CardSuit),
   get_cards_by_rank_and_suit(EncounteredAsteriskedCards, CardRank, CardSuit, []),
   deserialize_card(FirstAsteriskedCardAtom, CardsEncountered, Card),
   NewCardsEncountered = [Card | CardsEncountered], 
   NewEncounteredAsteriskedCards = [Card | EncounteredAsteriskedCards],
   NewHand = [Card | Hand],
   NewIncompleteMeld = [Card | IncompleteMeld],
   extract_unencountered_asterisked_cards(
                              RemainingCardAtoms, 
                              NewHand, 
                              NewCardsEncountered,
                              NewEncounteredAsteriskedCards, 
                              NewIncompleteMeld,
                              UpdatedCardsEncountered,
                              UpdatedEncounteredAsteriskedCards,
                              Meld,
                              UpdatedHand
                           ).

/* *********************************************************************
Rule Name: extract_non_asterisked_cards/7
Purpose: Deserialize and extract all the non-asterisked cards from a list of serialized non-asterisked cards
Parameters:
   Input:
      NonAsteriskedCards: The list of all serialized non-asterisked cards
      Hand: The incomplete deserialized hand
      CardsEncountered: The cards encountered so far
      Meld: The unfinished meld (with only asterisked cards present)
   Output:
      UpdatedHand: The updated hand
      UpdatedCardsEncountered: The updated list of cards encountered
      CompletedMeld: the finished meld with both asterisked and non-asterisked cards present
   Algorithm:
         1) For each meld in the serialized melds list
         2)    In the first meld in the list of serialized melds, separate out the asterisked and non-asterisked cards
         3)    deserialize and extract only the previously unencountered *asterisked* cards from the list of asterisked cards and add those to list of encountered cards and to hand
         4)    Deserialize and extract all the non-asterisked cards and add those to the list of encountered cards and to hand
         5)    Add *all* the deserialized asterisked cards (even the ones previously encountered) and the non-asterisked cards to form a meld
         6)    Add the formed meld to the deserialized melds storage
Assistance Received: none
********************************************************************* */ 
extract_non_asterisked_cards([], Hand, CardsEncountered, Meld, Hand, CardsEncountered, Meld).

extract_non_asterisked_cards([FirstCardAtom | RemainingCardAtoms], Hand, CardsEncountered, IncompleteMeld, UpdatedHand, UpdatedCardsEncountered, CompletedMeld) :-
   deserialize_card(FirstCardAtom, CardsEncountered, FirstCard),
   NewIncompleteMeld = [FirstCard | IncompleteMeld],
   NewHand = [FirstCard | Hand],
   NewCardsEncountered = [FirstCard | CardsEncountered],

   extract_non_asterisked_cards(RemainingCardAtoms, NewHand, NewCardsEncountered, NewIncompleteMeld, UpdatedHand, UpdatedCardsEncountered, CompletedMeld).




   
/* *********************************************************************
Rule Name: separate_asterisked_and_non_asterisked_cards/3
Purpose: Separates out all the asterisked cards and non-asterisked cards in a serialized meld into separate lists 
Parameters:
   Input:
      SerializedMeld: The serialized meld whose cards are to be separated out
   Output:
      AsteriskedCards: The list of asterisked cards
      NonAsteriskedCards: The list of non-asterisked cards
Assistance Received: none
********************************************************************* */ 
separate_asterisked_and_non_asterisked_cards(SerializedMeld, AsteriskedCards, NonAsteriskedCards) :-
   separate_asterisked_and_non_asterisked_cards(SerializedMeld, [], [], AsteriskedCards, NonAsteriskedCards).

separate_asterisked_and_non_asterisked_cards([], OldAsteriskedCards, OldNonAsteriskedCards, OldAsteriskedCards, OldNonAsteriskedCards).

separate_asterisked_and_non_asterisked_cards([FirstCard | RemainingCards], OldAsteriskedCards, OldNonAsteriskedCards, NewAsteriskedCards, NewNonAsteriskedCards) :-
   card_atom_type(FirstCard, asterisked),
   UpdatedAsteriskedCards = [FirstCard | OldAsteriskedCards],
   separate_asterisked_and_non_asterisked_cards(RemainingCards, UpdatedAsteriskedCards, OldNonAsteriskedCards, NewAsteriskedCards, NewNonAsteriskedCards).

separate_asterisked_and_non_asterisked_cards([FirstCard | RemainingCards], OldAsteriskedCards, OldNonAsteriskedCards, NewAsteriskedCards, NewNonAsteriskedCards) :-
   card_atom_type(FirstCard, non_asterisked),
   UpdatedNonAsteriskedCards = [FirstCard | OldNonAsteriskedCards],
   separate_asterisked_and_non_asterisked_cards(RemainingCards, OldAsteriskedCards, UpdatedNonAsteriskedCards, NewAsteriskedCards, NewNonAsteriskedCards).


/* *********************************************************************
Rule Name: deserialize_card/3
Purpose: Deserialize a card atom into a card id. If the card id is a part of the cards encoutered, then uses the alternative id for the card (since there are two
            cards of each type in Pinochle)
Parameters:
   Input:
      CardAtom: The serialized card representation
      CardsEncountered: The cards already encountered during deserialization
   Output:
      Card: The deserialized card
Assistance Received: none
********************************************************************* */ 
deserialize_card(CardAtom, _, Card) :-
   card_atom_to_id(CardAtom, TestCard),
   suit_index(TestCard, _),
   Card = TestCard.

deserialize_card(CardAtom, CardsEncountered, Card) :-
   card_atom_to_id(CardAtom, TestCard),
   \+ (card_list_contains_card(CardsEncountered, TestCard)),
   Card is TestCard.

deserialize_card(CardAtom, CardsEncountered, Card) :-
   card_atom_to_id(CardAtom, TestCard),
   card_list_contains_card(CardsEncountered, TestCard),
   num_of_unique_cards_in_game(NumOfUniqueCardsInGame),
   Card is TestCard + NumOfUniqueCardsInGame.



/* *********************************************************************
Rule Name: deserialize_list_of_cards/3
Purpose: Deserialize a list of cards
Parameters:
   Input:
      SerializedCards: The list of serialized cards
      CardsEncountered: The cards already encountered during deserialization
   Output:
      DeserializedCards: The list of deserialized cards
Assistance Received: none
********************************************************************* */ 
deserialize_list_of_cards([], _, []).

deserialize_list_of_cards([FirstCardAtom | RemainingCardAtoms], CardsEncountered, DeserializedCards) :-
   deserialize_list_of_cards(RemainingCardAtoms, CardsEncountered, IncompleteDeserializedCards),
   deserialize_card(FirstCardAtom, CardsEncountered, FirstCard),
   DeserializedCards = [FirstCard | IncompleteDeserializedCards].



/* *********************************************************************
Rule Name: get_suit_from_trump_card/2
Purpose: Retrieve the trump suit from the trump card (if the trump card actually contains a suit, simply return the suit)
Parameters:
   Input:
      TrumpCard/TrumpSuit: The trump card (or trump suit)
   Output:
      TrumpSuit: The trump suit
Assistance Received: none
********************************************************************* */ 
get_suit_from_trump_card(TrumpSuit, TrumpSuit) :-
   suit_index(TrumpSuit, _).

get_suit_from_trump_card(TrumpCard, TrumpSuit) :-
   get_card_suit(TrumpCard, TrumpSuit).


