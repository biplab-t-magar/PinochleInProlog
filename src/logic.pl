:- module(logic, [
                  suggest_lead_card/5, 
                  suggest_chase_card/6,
                  suggest_meld/6
                  ]).

:- use_module(cards).
:- use_module(card_pile).
:- use_module(melds).



/* *****************************************************************************************************
Clauses related to the logic for lead card suggestion/computer lead card move
******************************************************************************************************* */


/* *********************************************************************
Rule Name: suggest_lead_card/5
Purpose: Create a suggestion for what lead card to play in a round along with the logic for playing the card
Parameters:
   Input:
      Hand: The player's hand
      MeldPlayed: The melds played by the player   
      TrumpSuit: The trump suit of the round
   Output: 
      LeadCard: The card that is suggested to be played
      Logic: The logic for suggesting the card
Algorithm:
      1) Find the cards in the hand that best preserve the melds in hand
      2) Find the strongest cards from that list (i.e. find the cards that have the highest rank and are of trump suit if possible)
      3) Generate the logic based on what card was chosen as lead card suggestion 
Assistance Received: none
********************************************************************* */

suggest_lead_card(Hand, MeldsPlayed, TrumpSuit, LeadCard, Logic) :-
   cards_that_best_preserve_melds(Hand, MeldsPlayed, TrumpSuit, BestLeadCards),
   find_strongest_card(BestLeadCards, TrumpSuit, LeadCard, Reason),
   lead_card_logic(Reason, Logic).


/* *********************************************************************
Rule Name: find_strongest_card/4
Purpose: Find the strongest card in the given list (i.e. the card with the highest rank and of trump suit if possible)
Parameters:
   Input:
      CardPile: The list of cards from which to find the strongest card
      TrumpSuit: The trump suit for the round
   Output: 
      StrongestCard: The strongest card in the list
      Reason: The logic for suggesting the card
Algorithm:
      1) If there is only card in the list, throw that card
      2) If there is only one trump suit card in the list, throw that card
      3) If there is are multiple trump suit cards in the list, throw the one with the highest rank
      4) If there are no trump suit cards, throw that card with the highest rank
Assistance Received: none
********************************************************************* */
%if only a single card in card pile 
find_strongest_card([StrongestCard], _, StrongestCard, only_candidate_card).

%if empty list
find_strongest_card([], _, -1, no_candidate_cards).

find_strongest_card(CardPile, TrumpSuit, StrongestCard, Reason) :-
   %if there is one and only one trump suit cards in the pile, then that card is the strongest card
   get_cards_by_suit(CardPile, TrumpSuit, [StrongestCard]),
   %if there is only one card in the pile/list, then clause succeeds
   Reason = card_is_trump_suit.

find_strongest_card(CardPile, TrumpSuit, StrongestCard, Reason) :-
   %if there are multiple trump suit cards in the pile, then the card with the highest rank from among all the cards of trump suit is the strongest card
   get_cards_by_suit(CardPile, TrumpSuit, AllTrumpCardsInCardPile),
   \+ (AllTrumpCardsInCardPile = []),

   %assign any trump card with the highest rank as the strongest card
   get_cards_with_highest_rank(AllTrumpCardsInCardPile, [StrongestCard | _]),
   Reason = card_is_highest_ranked_trump_suit.

find_strongest_card(CardPile, _, StrongestCard, Reason) :-
   %if there are no trump suit cards, then assign any card with the highest rank as the strongest card
   get_cards_with_highest_rank(CardPile, [StrongestCard | _]),
   Reason = card_has_highest_rank.


/*
   Generate lead card logic string based on the logic code represented by an atom generated in the find_strongest_card clause
*/

lead_card_logic(
      only_candidate_card, 
      "throwing this card would preserve the most favorable hand, which has the most high-value melds"
   ).

lead_card_logic(
      card_is_trump_suit, 
      "throwing this card would preserve the most favorable hand (meld-wise) and it would increase the chance of winning because it is of trump suit"
   ).

lead_card_logic(
      card_is_highest_ranked_trump_suit, 
      "throwing this card would preserve the most favorable hand (meld-wise) and it would increase the chance of winning because it is the highest-ranked card with trump suit"
   ).

lead_card_logic(
      card_has_highest_rank, 
      "throwing this card would preserve the most favorable hand (meld-wise) and it would increase the chance of winning because it has a high rank."
   ).

lead_card_logic(
      no_candidate_cards, 
      "no card to throw on an empty hand"
   ).


/* *********************************************************************
Rule Name: cards_that_best_preserve_melds/6
Purpose: Generates the list of cards that, when thrown, still preserve the melds in hand as best as possible
Parameters:
   Input:
      Hand: The player's hand
      MeldPlayed: The melds played by the player
      TrumpSuit: The Trump suit for the round
   Output: 
      BestCards: The list of cards that would preserve the best melds
Algorithm:
      1) Assign the first card to currently be the provisional best card in hand
      2) Assign n = 2
      3) If n is out of bounds in regards to the current number of cards in hand, then hte best cards are the provisional best cards
      4) Assign nth card in hand to be the current competing card
      5) Generate two new hands, one from removing the provisional best card in hand and he other from removing the current competing card
      6) If the first hand has better melds than the second one, 
            7) Keep the first hand as the best hand
      8) If the first hand has worse melds than the second one,
            9) Make the current competing card the new provisional best card
            10) Make the current competing hand into the new best hand
      10) If the first hand is as good as the second one in terms of melds
            11) Add the current competing card to the list of best cards
      12) Make the competing hand into the n + 1th card in hand
Assistance Received: none
********************************************************************* */

cards_that_best_preserve_melds([], _, _, []).

cards_that_best_preserve_melds(Hand, MeldsPlayed, TrumpSuit, BestCards) :-
   remove_card_by_position(Hand, 0, ProvisionalBestHand, ProvisionalBestCard),
   remove_card_by_position(Hand, 1, CompetingHand, CompetingBestCard),
   cards_that_best_preserve_melds(Hand, MeldsPlayed, TrumpSuit, ProvisionalBestHand, [ProvisionalBestCard], CompetingHand, CompetingBestCard, 2, BestCards).


cards_that_best_preserve_melds(Hand, _, _, _, ProvisionalBestCards, _, _, Index, ProvisionalBestCards) :-
   %stop recursion if the index exceeds the Hand and assign the provisional best cards to be the BestCards
   length(Hand, HandSize),
   Index >= HandSize.

cards_that_best_preserve_melds(Hand, MeldsPlayed, TrumpSuit, ProvisionalBestHand, ProvisionalBestCards, CompetingHand, CompetingBestCard, Index,  BestCards) :-
   %find out which hand between the provisional best hand and the competing hand is better
   compare_hands_for_melds(ProvisionalBestHand, CompetingHand, MeldsPlayed, TrumpSuit, Winner),

   %update the provisional best hand and the provisional best cards according to which hand is better
   update_provisional_best_hand(Winner, ProvisionalBestHand, ProvisionalBestCards, CompetingHand, CompetingBestCard, NewProvisionalBestHand, NewProvisionalBestCards),

   %then move on the next competing hand and card
   remove_card_by_position(Hand, Index, NewCompetingHand, NewCompetingBestCard),

   %move on to the next iteration of comparison
   NextIndex is Index + 1,
   cards_that_best_preserve_melds(Hand, MeldsPlayed, TrumpSuit, NewProvisionalBestHand, NewProvisionalBestCards, NewCompetingHand, NewCompetingBestCard, NextIndex, BestCards).



/* *********************************************************************
Rule Name: update_provisional_best_hand/7
Purpose: Updates the provisional best hand and provisional best cards list by looking at what hand is better
Parameters:
   Input:
      BetterHad: Atom denoting which of the two hands compared was better
      ProvisionalBestHand: The provisional best hand
      ProvisionalBestCards: The card(s) in hand thrown to obtain the provisional best hand or its equivalent
      CompetingHand: The hand currently being compared to the provisional best hand
      CompetingBestCard: The card in hand thrown to obtain the competing hand
   Output: 
      NewProvisionalBestHand: The updated provisional best hand
      NewBestCards = The updated list of best cards
Assistance Received: none
********************************************************************* */
%if the provisional best hand is better than the competing hand, then keep the new provisional best hand and new provisional best cards unchanged
update_provisional_best_hand(first_hand, ProvisionalBestHand, ProvisionalBestCards, _, _, ProvisionalBestHand, ProvisionalBestCards).

%if the competing hand is better than the provisional best hand, then assign the new provisional best hand to be the current competing hand
%   and the new provisional best cards to be comprised of the current competing best card
update_provisional_best_hand(second_hand, _, _, CompetingHand, CompetingBestCard, CompetingHand, [CompetingBestCard]).

%if the competing hand draws with the provisional best hand, then add the current competing hand to the provisional list of best cards
update_provisional_best_hand(draw, ProvisionalBestHand, ProvisionalBestCards, _, CompetingBestCard, ProvisionalBestHand, [CompetingBestCard|ProvisionalBestCards]).



/* *****************************************************************************************************
Clauses related to the logic for chase card suggestion/computer chase card move
******************************************************************************************************* */


/* *********************************************************************
Rule Name: suggest_chase_card/6
Purpose: Create a suggestion for what chase card to play in a round along with the logic for playing the card
Parameters:
   Input:
      Hand: The player's hand
      MeldPlayed: The melds played by the player   
      TrumpSuit: The trump suit of the round
   Output: 
      LeadCard: The lead card played by the opponent
      ChaseCard: The card that is suggested to be played
      Logic: The logic for suggesting the card
Algorithm:
      1) If the lead card is of trump suit:
            2) Find all the trump suit cards in the players hand that have higher rank than the lead card
            3) If the list of all trump suit cards is not empty 
                  4)From the list of higher-ranked trump suit cards, get a list of those cards that preserve the better melds
                  5)From that list of cards, return the least ranked card
      6) If the lead card is not of trump suit:
            7) Get all the cards in hand with higher rank than the opponent's lead card 
            8) Get all the cards in hand with trump suit
            9) Append the first list to the second 
            10) From the appended list, find those cards that preserve the best melds
            11) From that list, return the weakest card
      12) If the lead card is not of trump suit:
            13) Find the list of all trump suit cards in hand
            14) If the list of all trump suit cards in hand is not empty:
                  15) Find the list of cards from the list of trump suit cards that best preserve melds
                  16) Return the list reanked card from this list of cards
      17) If none of the above conditions are fulfilled:
            18) Find the list of cards that preserve the best melds 
            19) From that list of cards, return the weakest card
Assistance Received: none
********************************************************************* */

suggest_chase_card(Hand, MeldsPlayed, TrumpSuit, LeadCard, ChaseCard, Logic) :-
   get_card_suit(LeadCard, LeadCardSuit),
   %if the lead card is of trump suit
   LeadCardSuit = TrumpSuit,

   get_card_rank(LeadCard, LeadCardRank),
   get_cards_by_suit(Hand, TrumpSuit, CardsWithTrumpSuit),
   get_all_cards_with_higher_rank(CardsWithTrumpSuit, LeadCardRank, TrumpCardsWithHigherRank),

   %if the the list of cards with higher rank is not empty
   TrumpCardsWithHigherRank \= [],
   %from the list of cards with higher rank, get the list of all cards that best preserve melds in hand
   cards_that_best_preserve_melds(TrumpCardsWithHigherRank, MeldsPlayed, TrumpSuit, BestChaseCards),

   %from the list of cards that have higher rank and best preserve melds, throw the least ranked card 
   get_least_ranked_card(BestChaseCards, ChaseCard),
   Logic = "this card is the least ranked trump-suit card that would also beat the opponent's trump-suit lead card, while also preserving the best melds in hand as far as possible".

suggest_chase_card(Hand, MeldsPlayed, TrumpSuit, LeadCard, ChaseCard, Logic) :-
   get_card_rank(LeadCard, LeadCardRank),
   get_card_suit(LeadCard, LeadCardSuit),
   get_cards_by_suit(Hand, LeadCardSuit, CardsWithLeadCardsSuit),

   LeadCardSuit \= TrumpSuit,
   get_all_cards_with_higher_rank(CardsWithLeadCardsSuit, LeadCardRank, CardsWithHigherRank),

   %if the the list of cards with higher rank is not empty
   CardsWithHigherRank \= [],

   %get the list of all trump suit cards in hand
   %any card with a trump suit will beat the opponents lead card
   get_cards_by_suit(Hand, TrumpSuit, CardsWithTrumpSuit),

   append(CardsWithHigherRank, CardsWithTrumpSuit, CardsThatCanBeatLeadCard),

   %from the list of cards that can beat the lead card, get the list of all cards that best preserve melds in hand
   cards_that_best_preserve_melds(CardsThatCanBeatLeadCard, MeldsPlayed, TrumpSuit, BestChaseCards),

   %from the list of cards that can beat the lead card and best preserve melds, throw the least ranked card 
   find_weakest_card(BestChaseCards, TrumpSuit, ChaseCard),
   Logic = "this card is the least valuable card that would also beat the opponent's lead card, while also preserving the best melds in hand as far as possible".

suggest_chase_card(Hand, MeldsPlayed, TrumpSuit, LeadCard, ChaseCard, Logic) :-
   %if the lead card is not of trump suit
   get_card_suit(LeadCard, LeadCardSuit),
   LeadCardSuit \= TrumpSuit,

   %then retrieve all cards of trump suit from hand
   get_cards_by_suit(Hand, TrumpSuit, CardsWithTrumpSuit),

   %if there are trump suit cards in hand
   CardsWithTrumpSuit \= [],
   %from the list of cards with trump suit, get the list of all cards that best preserve melds in hand
   cards_that_best_preserve_melds(CardsWithTrumpSuit, MeldsPlayed, TrumpSuit, BestChaseCards),

   %from the list of cards that are of trump suit and that best preserve melds, throw the least ranked card 
   get_least_ranked_card(BestChaseCards, ChaseCard),
   Logic = "this card is the least ranked trump-suit card that can beat the opponent's non-trump-suit card while also preserving the best melds in hand as far as possible".

suggest_chase_card(Hand, MeldsPlayed, TrumpSuit, _, ChaseCard, Logic) :-
   %if there is not possible way to win the turn
   %find all the cards in the hand that preserve the best melds
   cards_that_best_preserve_melds(Hand, MeldsPlayed, TrumpSuit, BestChaseCards),

   %from the list of cards that best preserve melds, throw the weakest card 
   find_weakest_card(BestChaseCards, TrumpSuit, ChaseCard),
   Logic = "there is no possible way to beat the opponent's lead card and this chase card is the least valuable card from among the cards that preserve the best melds in hand".
   



/* *********************************************************************
Rule Name: find_weakest_card/3
Purpose: Find the weakest card from a list of cards, defined as the card with the lowest rank and not of trump suit, if possible
Parameters:
   Input:
      CardList: The list of cards from which the weakest card is to be found
      TrumpSuit: The trump suit of the round
   Output: 
      WeakestCard: The weakest card from the list of cards
Assistance Received: none
********************************************************************* */
find_weakest_card([], _, -1).

find_weakest_card([FirstCard | RemainingCards], TrumpSuit, WeakestCard) :-
   find_weakest_card(RemainingCards, TrumpSuit, FirstCard, WeakestCard).

find_weakest_card([], _, ProvisionalWeakestCard, ProvisionalWeakestCard).

find_weakest_card([FirstCard | RemainingCards], TrumpSuit, ProvisionalWeakestCard, WeakestCard) :-
   weaker_or_same_valued_card(FirstCard, ProvisionalWeakestCard, TrumpSuit, NewProvisionalWeakestCard),
   find_weakest_card(RemainingCards, TrumpSuit, NewProvisionalWeakestCard, WeakestCard).

   
/* *********************************************************************
Rule Name: weaker_or_same_valued_card/3
Purpose: Find the weaker card between two cards, weakest being defined as the card with the lowest rank and not of trump suit, if possible
Parameters:
   Input:
      Card1: The first card to be compared 
      Card2: The second card to be compared
      TrumpSuit: The trump suit of the round
   Output: 
      WeakerCard: The weaker card from among the two cards. If they are both equally weak, return either one
Assistance Received: none
********************************************************************* */
weaker_or_same_valued_card(Card1, Card2, TrumpSuit, Card2) :-
   get_card_suit(Card1, Card1Suit),
   get_card_suit(Card2, Card2Suit),
   Card1Suit = TrumpSuit,
   Card2Suit \= TrumpSuit.

weaker_or_same_valued_card(Card1, Card2, TrumpSuit, Card1) :-
   get_card_suit(Card1, Card1Suit),
   get_card_suit(Card2, Card2Suit),
   Card2Suit = TrumpSuit,
   Card1Suit \= TrumpSuit.

weaker_or_same_valued_card(Card1, Card2, _, WeakerCard) :-
   %if both the cards are of trump suit or both are not of trump suit
   card_with_same_or_less_rank(Card1, Card2, WeakerCard).



/* *****************************************************************************************************
Clauses related to the logic for meld suggestion/computer meld move
******************************************************************************************************* */


/* *********************************************************************
Rule Name: suggest_meld/6
Purpose: Create a suggestion for what meld to play in a round along with the logic for playing the meld
Parameters:
   Input:
      Hand: The player's hand
      MeldPlayed: The melds played by the player   
      TrumpSuit: The trump suit of the round
   Output: 
      MeldToPlay: The suggested meld to be played
      MeldCards: The cards comprising the suggested meld
      Logic: The logic for suggesting the meld
Algorithm:
      1) If there are no melds in hand, return -1 for the meld to play and empty list for the meld cards
      2) If there are possible melds in hand:
         3) Get the list of all melds in hand
         4) Return the most profitable meld in hand (by points each meld is worth)
Assistance Received: none
********************************************************************* */

suggest_meld(Hand, MeldsPlayed, TrumpSuit, MeldToPlay, MeldCards, Logic) :-
   total_number_of_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, TotalNumOfMeldsInHand),
   suggest_meld(TotalNumOfMeldsInHand, Hand, MeldsPlayed, TrumpSuit, MeldToPlay, MeldCards, Logic).

suggest_meld(0, _, _, _, -1, [], Logic) :-
   Logic = "there are no possible melds in hand.".

suggest_meld(_, Hand, MeldsPlayed, TrumpSuit, MeldToPlay, MeldCards, Logic) :-
   get_all_melds_in_hand(Hand, MeldsPlayed, TrumpSuit, AllMeldsInHand),
   get_most_profitable_meld_in_hand(AllMeldsInHand, MeldToPlay, MeldInstances),
   %suggest the first instance of the meld to play
   [MeldCards | _ ] = MeldInstances, 
   Logic = "playing this meld will yield the highest possible points".


/* *********************************************************************
Rule Name: get_most_profitable_meld_in_hand/6
Purpose: Create a suggestion for what meld to play in a round along with the logic for playing the meld
Parameters:
   Input:
      AllMeldsInHand: The list containing the instances of all possible melds in hand
   Output: 
      MeldToPlay: The meld that would generate the most points
      MeldInstances: The instances of the meld that would generate the most points
Algorithm:
      1) Assign the first meld type as the provisional most profitable meld
      2) If there are no more melds to play with then the provisional meld is the most profitable
      3) If the current meld being compared to the provisional meld has no instances in hand, move on to the next meld type to compare
      4) If the provisional most profitable meld does not have an instance, make the next meld type be the provisional meld
      5) If the meld being compared to the provisional meld is more profitable:
            6) Change the provisional meld to the new meld 
            7) Empty out the previous provisonal meld instance list and add the meld instances of the new meld to the list
      8) If the next meld being compared to type is just as profitable If the current meld being compared with the provisional meld is not worth more than the:
            9) Keep the provisional meld and provisional meld instances as they are
      10) Repeat from step 2.
Assistance Received: none
********************************************************************* */
get_most_profitable_meld_in_hand([InstancesOfFirstMeldType | InstancesOfRemainingMeldType], MeldToPlay, MeldInstances) :-
   meld_index(ProvisionalMeldToPlay, 0),
   get_most_profitable_meld_in_hand(InstancesOfRemainingMeldType, 1, ProvisionalMeldToPlay, InstancesOfFirstMeldType, MeldToPlay, MeldInstances).
   
%if there care no more meld to compare with 
get_most_profitable_meld_in_hand([], _, ProvisionalMeldToPlay, ProvisionalMeldInstances,ProvisionalMeldToPlay, ProvisionalMeldInstances).
   
%if the the current meld has no instances, stick with the provisional meld
get_most_profitable_meld_in_hand([[] | InstancesOfRemainingMeldType], MeldIndex, ProvisionalMeldToPlay, ProvisionalMeldInstances, MeldToPlay, MeldInstances) :-
   NextMeldIndex is MeldIndex + 1,
   get_most_profitable_meld_in_hand(InstancesOfRemainingMeldType, NextMeldIndex, ProvisionalMeldToPlay, ProvisionalMeldInstances, MeldToPlay, MeldInstances).
   
%if the the provisional meld has no instances, move on to another provisional meld
get_most_profitable_meld_in_hand([InstancesOfFirstMeldType | InstancesOfRemainingMeldType], MeldIndex, _, [], MeldToPlay, MeldInstances) :-
   meld_index(Meld, MeldIndex),
   NextMeldIndex is MeldIndex + 1,
   get_most_profitable_meld_in_hand(InstancesOfRemainingMeldType, NextMeldIndex, Meld, InstancesOfFirstMeldType, MeldToPlay, MeldInstances).


get_most_profitable_meld_in_hand([InstancesOfFirstMeldType | InstancesOfRemainingMeldType], MeldIndex, ProvisionalMeldToPlay, _, MeldToPlay, MeldInstances) :-
   meld_index(Meld, MeldIndex),
   %if the current meld is worth more than the provisional meld
   meld_worth_more_points(Meld, ProvisionalMeldToPlay, Meld),
   NextMeldIndex is MeldIndex + 1,
   get_most_profitable_meld_in_hand(InstancesOfRemainingMeldType, NextMeldIndex, Meld, InstancesOfFirstMeldType, MeldToPlay, MeldInstances).

get_most_profitable_meld_in_hand([_ | InstancesOfRemainingMeldType], MeldIndex, ProvisionalMeldToPlay, ProvisionalMeldInstances, MeldToPlay, MeldInstances) :-
   %if the current meld is not worth more than the provisional meld or if it is worth the same
   NextMeldIndex is MeldIndex + 1,
   get_most_profitable_meld_in_hand(InstancesOfRemainingMeldType, NextMeldIndex, ProvisionalMeldToPlay, ProvisionalMeldInstances, MeldToPlay, MeldInstances).

















