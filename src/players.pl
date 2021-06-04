:- module(players, [get_player_string/2, play_lead_card/8, play_chase_card/9, play_meld/10]).

:- use_module(card_pile).
:- use_module(cards).
:- use_module(logic).
:- use_module(melds).




/* *********************************************************************
Rule Name: get_player_string/2
Purpose: converts atomic representation of player in to string representation and vice versa
Parameters:
   Input:
      PlayerAtom: atom representation of player
   Output: 
      PlayerString: string representation of player
Assistance Received: none
********************************************************************* */
get_player_string(human, "Human").
get_player_string(computer, "Computer").


/* ********************************************************************************************************
Clauses for playing lead and chase cards
******************************************************************************************************** */

/* *********************************************************************
Rule Name: play_lead_card/8
Purpose: makes the player whose turn it is play the lead card
Parameters:
   Input:
      Player: the player whose turn it is
      TrumpSuit: The trump suit of the round
      ComputerHand: The computer's hand
      ComputerMelds: The melds played by the computer
      HumansHand: The Humans's hand      
   Output: 
      UpdatedComputerHand: the computer's hand after the player plays a lead card
      UpdatedHumanHand: the human's hand after the player plays the lead card
      LeadCard: the lead card played by the player
Assistance Received: none
********************************************************************* */
play_lead_card(human, _, ComputerHand, _, HumanHand, ComputerHand, UpdatedHumanHand, LeadCard) :-
   %ask human to pick a card to throw
   prompt_card_throw(HumanHand, CardPosition),

   %remove the card chosen by the human from his/her hand and store that card as the lead card
   remove_card_by_position(HumanHand, CardPosition, UpdatedHumanHand, LeadCard),

   %declare card played as lead card
   card_string(LeadCard, LeadCardString),
   format("~nYou chose to play ~s as your lead card.~n~n", [LeadCardString]).
   

play_lead_card(computer, TrumpSuit, ComputerHand, ComputerMelds, HumanHand, UpdatedComputerHand, HumanHand, LeadCard) :-
   %decide what card to play as lead card
   suggest_lead_card(ComputerHand, ComputerMelds, TrumpSuit, LeadCard, Logic),

   %remove the suggested lead card from computer hand
   remove_card(ComputerHand, LeadCard, UpdatedComputerHand),

   %give reasoning for playing the lead card 
   card_string(LeadCard, LeadCardString),
   format("~nThe computer chose to play ~s as its lead card because ~s.~n~n", [LeadCardString, Logic]).




/* *********************************************************************
Rule Name: play_chase_card/8
Purpose: makes the player whose turn it is play the chase card
Parameters:
   Input:
      Player: the player whose turn it is
      LeadCard: the lead card played by the opponent
      TrumpSuit: The trump suit of the round
      ComputerHand: The computer's hand
      ComputerMelds: The melds played by the computer
      HumansHand: The Humans's hand      
   Output: 
      UpdatedComputerHand: the computer's hand after the player plays a chase card
      UpdatedHumanHand: the human's hand after the player plays the chase card
      ChaseCard: the chase card played by the player
Assistance Received: none
********************************************************************* */
play_chase_card(human, LeadCard, _, ComputerHand, _, HumanHand, ComputerHand, UpdatedHumanHand, ChaseCard) :-
   %declare the lead card played by the opponent
   card_string(LeadCard, LeadCardString),
   format("~nThe computer chose to play ~s as its lead card.~n", [LeadCardString]),

   %ask human to pick a card to throw
   prompt_card_throw(HumanHand, CardPosition),

   %remove the card chosen by the human from his/her hand and store that card as the chase card
   remove_card_by_position(HumanHand, CardPosition, UpdatedHumanHand, ChaseCard),

   %declare card played as chase card
   card_string(ChaseCard, ChaseCardString),
   format("~n~nYou chose to play ~s as your chase card.~n~n", [ChaseCardString]).
   

play_chase_card(computer, LeadCard, TrumpSuit, ComputerHand, ComputerMelds, HumanHand, UpdatedComputerHand, HumanHand, ChaseCard) :-
   %decide what card to play as chase card
   suggest_chase_card(ComputerHand, ComputerMelds, TrumpSuit, LeadCard, ChaseCard, Logic),

   %remove the suggested chase card from computer hand
   remove_card(ComputerHand, ChaseCard, UpdatedComputerHand),

   %give reasoning for playing the chase card 
   card_string(ChaseCard, ChaseCardString),
   format("~nThe computer chose to play ~s as its chase card because ~s.~n~n", [ChaseCardString, Logic]).



/* *********************************************************************
Rule Name: prompt_card_throw/2
Purpose: Prompts the user on what card to throw
Parameters:
   Input:
      Hand: The player's hand      
   Output:
      CardPosition: The position in hand of the card to play 
Assistance Received: none
********************************************************************* */
prompt_card_throw(Hand, CardPosition) :-
   write("What card do you want to throw?"), nl,
   write("  Enter the card position (first card has position 0): "), nl,

   %read user input
   read_string(user_input, "\n", "\r", _, StringInput),
   %convert string to position
   position_string_to_position(Hand, StringInput, CardPosition).


/* *********************************************************************
Rule Name: position_string_to_position/3
Purpose: Converts position of card in string form into numeric form
Parameters:
   Input:
      Hand: The player's hand      
   Output:
      CardPosition: The position in hand of the card to play 
Assistance Received: none
********************************************************************* */
position_string_to_position(Hand, String, Position) :-
   number_string(Position, String),
   Position >= 0,
   length(Hand, HandSize),
   Position < HandSize.

position_string_to_position(Hand, _, Position) :-
   length(Hand, HandSize),
   LastIndex is HandSize - 1,
   format("~nInvalid input. You must enter a number from 0 to ~w. Please try again.~n", [LastIndex]),
   prompt_card_throw(Hand, Position).



/* ********************************************************************************************************
Clauses to play melds
******************************************************************************************************** */

/* *********************************************************************
Rule Name: play_meld/10
Purpose: makes the player whose turn it is play a meld
Parameters:
   Input:
      Player: the player whose turn it is to play a meld
      TrumpSuit: The trump suit of the round
      ComputerHand: The computer's hand
      ComputerMelds: The melds played by the computer
      HumansHand: The Humans's hand      
      HumanMelds: The melds played by the human
   Output: 
      UpdatedComputerMelds: the computer's melds after the player plays a meld
      UpdatedHumanMelds: the human's melds after the player plays a meld
      MeldPointsForComputer: the points won by the computer by playing a meld
      MeldPointsForHuman: the points won by the human by playing a meld
Assistance Received: none
********************************************************************* */
play_meld(human, TrumpSuit, _, ComputerMelds, HumanHand, HumanMelds, ComputerMelds, UpdatedHumanMelds, 0, MeldPointsForHuman) :-
   total_number_of_melds_in_hand(HumanHand, HumanMelds, TrumpSuit, TotalNumOfMeldsInHand),
   play_meld(TotalNumOfMeldsInHand, human, TrumpSuit, _, _, HumanHand, HumanMelds, _, UpdatedHumanMelds, 0, MeldPointsForHuman).

play_meld(computer, TrumpSuit, ComputerHand, ComputerMelds, _, HumanMelds, UpdatedComputerMelds, HumanMelds, MeldPointsForComputer, 0) :-
   total_number_of_melds_in_hand(ComputerHand, ComputerMelds, TrumpSuit, TotalNumOfMeldsInHand),
   play_meld(TotalNumOfMeldsInHand, computer, TrumpSuit, ComputerHand, ComputerMelds, _, _, UpdatedComputerMelds, _, MeldPointsForComputer, 0).

play_meld(0, human, _, _, _, _, HumanMelds, _, HumanMelds, 0, 0) :-
   write("There are no playable melds in your hand. Moving on to the next turn..."), nl, nl.

play_meld(0, computer, _, _, ComputerMelds, _, _, ComputerMelds, _, 0, 0) :-
   write("There are no playable melds in the computer's hand. Moving on to the next turn..."), nl, nl.

play_meld(_, human, TrumpSuit, _, _, HumanHand, HumanMelds, _, UpdatedHumanMelds, 0, MeldPointsForHuman) :-
   prompt_meld_move(HumanHand, TrumpSuit, HumanMelds), 
   write("Separately enter the positions (pressing enter after each card) of all the cards you would like to play for your meld. "),
   write("Enter a period '.' once you are done entering positions:"), nl,
   get_card_positions_for_meld(HumanHand, HumanMelds, TrumpSuit, [], "", PositionsList),
   get_cards_from_given_positions(HumanHand, PositionsList, Cards),
   
   %determine the meld type of the cards selected by the player
   create_meld(Cards, TrumpSuit, MeldType),
   meld_string(MeldType, MeldString),
   format("~nYou played a ~s for your meld.~n", [MeldString]),

   %add the created meld to the players store
   add_meld_to_storage(HumanMelds, Cards, MeldType, UpdatedHumanMelds),
   meld_points(MeldType, MeldPointsForHuman),
   format("You won ~w points for playing a ~s meld.~n~n", [MeldPointsForHuman, MeldString]).


play_meld(_, computer, TrumpSuit, ComputerHand, ComputerMelds, _, _, UpdatedComputerMelds, _, MeldPointsForComputer, 0) :-
   write("The computer won this turn, so it can create a meld."), nl,
   %get suggestion on what meld to play
   suggest_meld(ComputerHand, ComputerMelds, TrumpSuit, MeldToPlay, MeldCards, Logic),
   meld_string(MeldToPlay, MeldString),
   format("~nThe computer chose to play a ~s as its meld because ~s.~n", [MeldString, Logic]),
   add_meld_to_storage(ComputerMelds, MeldCards, MeldToPlay, UpdatedComputerMelds),
   meld_points(MeldToPlay, MeldPointsForComputer),
   format("The computer won ~w points for playing the meld.~n~n", [MeldPointsForComputer]).
   


/* *********************************************************************
Rule Name: get_card_positions_for_meld/6
Purpose: makes the player whose turn it is play a meld
Parameters:
   Input:
      HumanHand: The Humans's hand      
      MeldPlayed: The melds played by the human
      TrumpSuit: The trump suit of the round
      PositionsListInProgress: The list in progress of the positions entered by the user to specify meld
      PositionString: The position (in string form) entered by the user
   Output: 
      PositionsList: The final list of positions entered by the user to specify the meld

Assistance Received: none
********************************************************************* */
get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, "", PositionsList) :-
   %read user input
   read_string(user_input, "\n", "\r", _, StringInput),
   get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, StringInput, PositionsList).

get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, ".", PositionsList) :-
   %if the cards entered do not combined to form a valid meld
   get_cards_from_given_positions(HumanHand, PositionsListInProgress, Cards),
   create_meld(Cards, TrumpSuit, not_a_valid_meld),
   write("The cards you entered do not comprise a valid meld. Please try again:"), nl, nl,
   get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, [], "", PositionsList).

get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, ".", PositionsList) :-
   %if the cards entered do not combined to form a valid meld
   get_cards_from_given_positions(HumanHand, PositionsListInProgress, Cards),
   \+ (is_a_playable_meld(MeldsPlayed, Cards, TrumpSuit)),
   write("The list of cards you entered has at least one card that has already been used to create the same meld. Please try again:"), nl, nl,
   get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, [], "", PositionsList).
   
%if the user enters a ".", meaning that he is done entering card positions
get_card_positions_for_meld(_, _, _, PositionsListInProgress, ".", PositionsListInProgress).

%when the human enters a new valid position
get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, PositionString, PositionsList) :-
   number_string(Position, PositionString),
   Position >= 0,
   length(HumanHand, HandSize),
   Position < HandSize,
   NewPositionsListInProgress = [Position | PositionsListInProgress],
   get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, NewPositionsListInProgress, "", PositionsList).

%when the human enters an invalid position
get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, _, PositionsList) :-
   length(HumanHand, HandSize),
   LastIndex is HandSize - 1,
   format("~nInvalid input. You must enter a number from 0 to ~w. Please try another position or enter '.' if you are done.~n", [LastIndex]),
   get_card_positions_for_meld(HumanHand, MeldsPlayed, TrumpSuit, PositionsListInProgress, "", PositionsList).



/* *********************************************************************
Rule Name: prompt_meld_move/3
Purpose: Prompts the user whether to play a meld or to get help for a meld
Parameters:
   Input:
      HumanHand: The Humans's hand      
      TrumpSuit: The trump suit of the round
      HumanMelds: The melds played by the human
   Output: 

Assistance Received: none
********************************************************************* */
prompt_meld_move(HumanHand, TrumpSuit, HumanMelds) :-
   get_card_pile_string_with_positions(HumanHand, HumanHandString),
   suit_string(TrumpSuit, TrumpSuitString),
   get_melds_string(HumanMelds, HumanHand, MeldsString),
   write("You won this turn, so you can create a meld."), nl,
   write("Pick cards from your hand to create a meld:"), nl,
   write("-------------------------------------------------------------------------------------------------"), nl,
   format("Hand: ~s~n", [HumanHandString]),
   format("Trump Suit: ~s~n", [TrumpSuitString]),
   format("Melds Played: ~s~n", [MeldsString]),
   write("-------------------------------------------------------------------------------------------------"), nl, nl,
   write("Pick an action: "), nl,
   write("1.     Play a meld"), nl,
   write("2.     Ask for help"), nl,
   %read user input
   read_string(user_input, "\n", "\r", _, StringInput),
   atom_string(AtomInput, StringInput),
   meld_move_decistion(HumanHand, TrumpSuit, HumanMelds, AtomInput).


/* *********************************************************************
Rule Name: meld_move_decistion/4
Purpose: Moves on to the next stage of playing a meld depending on the user's input
Parameters:
   Input:
      HumanHand: The Humans's hand      
      TrumpSuit: The trump suit of the round
      HumanMelds: The melds played by the human
      Input: The human's input
   Output: 
Assistance Received: none
********************************************************************* */
%do nothing and move on to the stage for human to pick cards for meld
meld_move_decistion(_, _, _, '1').

meld_move_decistion(HumanHand, TrumpSuit, HumanMelds, '2') :-
   %get suggestion on what meld to play
   suggest_meld(HumanHand, HumanMelds, TrumpSuit, MeldToPlay, MeldCards, Logic),
   meld_string(MeldToPlay, MeldString),
   format("It is recommended that you use the following cards to create a ~s meld because ~s:~n", [MeldString, Logic]),
   get_card_pile_subset_string_with_positions(HumanHand, MeldCards, RecommendedCardsString),
   write(RecommendedCardsString), nl, nl.

meld_move_decistion(HumanHand, TrumpSuit, HumanMelds, _) :-
   write("You must enter either [1] or [2], and nothing else. Please try again."), nl,
   %read user input
   read_string(user_input, "\n", "\r", _, StringInput),
   atom_string(AtomInput, StringInput),
   meld_move_decistion(HumanHand, TrumpSuit, HumanMelds, AtomInput).



