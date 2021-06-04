:- module(pinochle, [start_game/0]).

:- use_module(cards).
:- use_module(deck).
:- use_module(card_pile).
:- use_module(melds).
:- use_module(logic).
:- use_module(players).
:- use_module(serialization).


/* *********************************************************************
Rule Name: start_game/0
Purpose: Starts a game of pinochle
Parameters:
   Input:
   Output: 
Assistance Received: none
********************************************************************* */
start_game :-
   write("Welcome to Pinochle!"), nl,
   prompt_new_or_load.
   

/* *********************************************************************
Rule Name: prompt_new_or_load/0
Purpose: Prompts the user whether to load or start a new game
Parameters:
   Input:
   Output: 
Assistance Received: none
********************************************************************* */
prompt_new_or_load :-
   write("Would you like to (1) start a new game or (2) load a previous game? Enter 'new' or 'load':"),nl,
   read_string(user_input, "\n", "\r", _, StringInput),
   atom_string(AtomInput, StringInput),
   begin_game(AtomInput).

/* *********************************************************************
Rule Name: begin_game/1
Purpose: Begins a game of Pinochle
Parameters:
   Input:
      NewOrLoaded: Specified whether it is a new game or a loaded game of pinochle to be begin
   Output: 
Assistance Received: none
********************************************************************* */
begin_game(new) :-
   nl,
   write("****************************************************************************************************"),nl,
   write("Beginning a new game of Pinochle."), nl,
   game_loop(0, 0, 1).

begin_game(load) :-
   nl,write("****************************************************************************************************"),nl,
   write("Loading a game of Pinochle."), nl,
   load_game.

begin_game(_):-
   nl, write("You must enter either 'new' or 'load'. Please try again."), nl,
   prompt_new_or_load.



/* *********************************************************************
Rule Name: load_game/0
Purpose: Loads a game of Pinochle from a file
Parameters:
   Input:
   Output: 
Assistance Received: none
********************************************************************* */
load_game :-
   write("Enter the name of the save file: "), nl,
   read_string(user_input, "\n", "\r", _, FileName),
   check_load_file_name(FileName),
   open(FileName,read,Stream),
   read(Stream, SaveFileData),

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
            HmnMelds),
   close(Stream),
   write("Game successfully loaded!"), nl, nl, nl,
   begin_loaded_round(
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
               HmnMelds
            ).


/* *********************************************************************
Rule Name: check_load_file_name/1
Purpose: Checks if the file name entered by a user is valid
Parameters:
   Input:
      FileName: the file name entered by the user
   Output: 
Assistance Received: none
********************************************************************* */
check_load_file_name(FileName) :-
   exists_file(FileName).
   
check_load_file_name(_) :-
   nl, write("The file does not exist. Try again."), nl,
   load_game.


/* *********************************************************************
Rule Name: prompt_save_game/14
Purpose: Prompts the user whether they want to save the game
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
Assistance Received: none
********************************************************************* */
prompt_save_game(
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
            HmnMelds
         ) :-
   write("Would you like to save your progress so far? (yes/no):"), nl,
   read_string(user_input, "\n", "\r", _, StringInput),
   %convert string to atom
   atom_string(Decision, StringInput),
   save_game(
            Decision,
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
            HmnMelds
            ).


/* *********************************************************************
Rule Name: save_game/15
Purpose: Saves the given game state
Parameters:
   Input:
      SaveGameOrNo: whether the user said yes to saving game (represented by atom yes or no)
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
Assistance Received: none
********************************************************************* */

save_game(no,_,_,_,_,_,_,_,_,_,_,_,_,_,_).

save_game(
         yes, 
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
   
   write("Enter the name of the save file: "), nl,
   read_string(user_input, "\n", "\r", _, FileName),

   serialize(
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
            HmnMelds, 
            SerializedData),
   

   open(FileName, write, Stream),
   write(Stream, SerializedData),
   write(Stream, '.'),
   close(Stream),

   nl, write("Game saved."), nl,
   halt(0).
   
save_game(
         _, 
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
         
   write("Invalid input. You must enter either 'yes or 'no'. Please try again."), nl,
   prompt_save_game(
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
         HmnMelds).



/* *********************************************************************
Rule Name: begin_loaded_round/14
Purpose: Begins a loaded round of pinochle
Parameters:
   Input:
      CmptrGameScr: the loaded computer game score
      HmnGameScr: the loaded human game score
      CmptrRndScore: the loaded computer round score
      HmnRndScore: the loaded human round score
      RoundNumber: the loaded round number
      Stock: the loaded stock
      TrmpCrd: the loaded trump card
      NextPlayer: the loaded next player
      CmptrHnd: the loaded computer hand
      HmnHnd: the loaded human hand
      CmptrCptrPile: the loaded computer capture pile
      HmnCptrPile: the loaded human capture pile
      CmptrMelds: the loaded melds played by the computer
      HmnMelds: the loaded melds played by the human
   Output: 
Assistance Received: none
********************************************************************* */
begin_loaded_round(
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
            HmnMelds
         ):-
   get_trump_suit_from_loaded_trump_card(TrmpCrd, TrumpSuit),

   round_loop(
            CmptrGameScr,
            HmnGameScr,
            CmptrRndScore,
            HmnRndScore,
            RoundNumber,
            Stock,
            TrmpCrd,
            TrumpSuit,
            NextPlayer,
            CmptrHnd,
            HmnHnd,
            CmptrCptrPile,
            HmnCptrPile,
            CmptrMelds,
            HmnMelds,
            NewCmptrRoundScr,
            NewHmnRoundScr
         ),
   %update new game score values and round number
   NewComputerGameScore is CmptrGameScr + NewCmptrRoundScr,
   NewHumanGameScore is HmnGameScr + NewHmnRoundScr,
   NewRoundNumber is RoundNumber + 1,

   display_round_results(NewComputerGameScore, NewHumanGameScore, NewCmptrRoundScr, NewHmnRoundScr),

   prompt_another_round(NewComputerGameScore, NewHumanGameScore, NewRoundNumber).


/* *********************************************************************
Rule Name: get_trump_suit_from_loaded_trump_card/2
Purpose: Gets the trump suit from a card (or simply returns the suit if the trump card actually contains a suit)
Parameters:
   Input:
      TrumpCard: the trump card (or trump suit)
      TrumpSuit: the suit contained in the trump card
   Output: 
Assistance Received: none
********************************************************************* */
get_trump_suit_from_loaded_trump_card(clubs, clubs).
get_trump_suit_from_loaded_trump_card(diamonds, diamonds).
get_trump_suit_from_loaded_trump_card(hearts, hearts).
get_trump_suit_from_loaded_trump_card(spades, spades).

get_trump_suit_from_loaded_trump_card(TrumpCard, TrumpSuit) :-
   get_card_suit(TrumpCard, TrumpSuit).


/* *********************************************************************
Rule Name: game_loop/3
Purpose: The game loop that prompts whether the player wants to play another round and starts a new round if the player answers yes
Parameters:
   Input:
      ComputerGameScore: the current game score of the computer
      HumanGameScore: the current game score of the human
      RoundNumber: the current round number
   Output: 
Assistance Received: none
********************************************************************* */
game_loop(ComputerGameScore, HumanGameScore, RoundNumber) :-
   %begin a new round with the given game scores and round number, retreiving the round scores once the round has ended
   begin_new_round(ComputerGameScore, HumanGameScore, RoundNumber, ComputerRoundScore, HumanRoundScore),

   %update new game score values and round number
   NewComputerGameScore is ComputerGameScore + ComputerRoundScore,
   NewHumanGameScore is HumanGameScore + HumanRoundScore,
   NewRoundNumber is RoundNumber + 1,

   display_round_results(NewComputerGameScore, NewHumanGameScore, ComputerRoundScore, HumanRoundScore),

   prompt_another_round(NewComputerGameScore, NewHumanGameScore, NewRoundNumber).

   
/* *********************************************************************
Rule Name: display_round_results/4
Purpose: Displays the result of the round that has just ended
Parameters:
   Input:
      ComputerGameScore: the current game score of the computer
      HumanGameScore: the current game score of the human
      ComputerRoundScore: the current round score of the computer
      HumanRoundScore: the current round score of the human
   Output: 
Assistance Received: none
********************************************************************* */
display_round_results(ComputerGameScore, HumanGameScore, ComputerRoundScore, HumanRoundScore):-
   nl,write("Round ended."),nl,nl,
   write("------------------------------------------------------------------------"),
   format("~nComputer's score this round: ~w~n", [ComputerRoundScore]),
   format("Your score this round: ~w~n", [HumanRoundScore]),
   format("~nComputer's total score so far: ~w~n", [ComputerGameScore]),
   format("Your total score so far: ~w~n", [HumanGameScore]),
   who_has_more_points(ComputerRoundScore, HumanRoundScore, Winner),
   declare_round_winner(Winner),
   write("------------------------------------------------------------------------").


/* *********************************************************************
Rule Name: prompt_another_round/3
Purpose: Prompts the player if they want to play another round
Parameters:
   Input:
      ComputerGameScore: the current game score of the computer
      HumanGameScore: the current game score of the human
      RoundNumber: the current round number
   Output: 
Assistance Received: none
********************************************************************* */
prompt_another_round(ComputerGameScore, HumanGameScore, RoundNumber) :-
   nl,write("Would you like to play another round? (Enter 'yes' or 'no'):"), nl,
   read_string(user_input, "\n", "\r", _, StringInput),
   atom_string(AtomInput, StringInput),
   play_another_round(AtomInput, ComputerGameScore, HumanGameScore, RoundNumber).

play_another_round(yes, ComputerGameScore, HumanGameScore, RoundNumber) :-
   game_loop(ComputerGameScore, HumanGameScore, RoundNumber).

play_another_round(no, ComputerGameScore, HumanGameScore, _) :-
   nl,write("------------------------------------------------------------------------"),
   format("~nGame ended.~n"),
   format("~nComputer's score: ~w~n", [ComputerGameScore]),
   format("Your score: ~w~n", [HumanGameScore]),
   who_has_more_points(ComputerGameScore, HumanGameScore, Winner),
   declare_game_winner(Winner),
   write("------------------------------------------------------------------------"), nl, nl,
   write("Thank you for playing Pinochle!"),nl,nl,
   halt(0).

play_another_round(_, ComputerGameScore, HumanGameScore, RoundNumber) :-
   write("You must enter either 'yes' or 'no'. Please try again."), nl,
   prompt_another_round(ComputerGameScore, HumanGameScore, RoundNumber).


/* *********************************************************************
Rule Name: declare_round_winner/1
Purpose: Generates a string declaring the winner of the round depending on who the winner is
Parameters:
   Input:
      Winner: the winner of the round
   Output: 
Assistance Received: none
********************************************************************* */
declare_round_winner(computer) :-
   nl, write("The computer won this round."), nl.

declare_round_winner(human) :-
   nl, write("You won this round."), nl.

declare_round_winner(draw) :-
   nl, write("The round resulted in a draw."), nl.


/* *********************************************************************
Rule Name: declare_game_winner/1
Purpose: Declares the winner of the game depending on who the winner is
Parameters:
   Input:
      Winner: the winner of the game
   Output: 
Assistance Received: none
********************************************************************* */
declare_game_winner(computer) :-
   nl, write("The computer won this game."), nl.

declare_game_winner(human) :-
   nl, write("You won this game."), nl.

declare_game_winner(draw) :-
   nl, write("The game resulted in a draw."), nl.


/* *********************************************************************
Rule Name: who_has_more_points/3
Purpose: Finds out which player has a higher score
Parameters:
   Input:
      ComputerScore: the computer's score
      HumanScore: the human's score 
   Output: 
      PlayerWithMorePoints: the player with more points
Assistance Received: none
********************************************************************* */
who_has_more_points(ComputerScore, HumanScore, computer) :-
   ComputerScore > HumanScore.

who_has_more_points(ComputerScore, HumanScore, human) :-
   HumanScore > ComputerScore.

who_has_more_points(ComputerScore, HumanScore, draw) :-
   ComputerScore = HumanScore.


/* *********************************************************************
Rule Name: begin_new_round/5
Purpose: Begins a new round of pinochle
Parameters:
   Input:
      CmptrGameScr: the computer's game score
      HmnGameScr: the human's game score 
      RoundNumber: the number of the round to begin
   Output: 
      CmptrRoundScr: The score obtained by the computer from this round
      HmnRoundScr: The score obtained by the human from this round
Assistance Received: none
********************************************************************* */
begin_new_round(CmptrGameScr, HmnGameScr, RoundNumber, CmptrRoundScr, HmnRoundScr) :-
   nl, write("------------------------------------------------------------------------"),nl,
   format("Starting round ~w...", [RoundNumber]),
   nl, write("-----------------------------------------------------------------------"),nl, nl,

   %generate the deck for the round and
   generate_deck(Deck),

   %distribute the cards from the deck to the players
   write("Distributing cards:"), nl, nl,
   distribute_cards(Deck, NewDeck, CmptrHnd, HmnHnd),

   %declare the trump card and assign the deck in to new list of cards stored in variable Stock
   take_card_from_top(NewDeck, Stock, TrmpCrd),
   get_card_suit(TrmpCrd, TrumpSuit),
   write("Picking card from the deck to find out the trump card."), nl,
   card_string(TrmpCrd, TrmpCrdString),
   format("The trump card for this round is ~s.~n~n", [TrmpCrdString]),

   %find out who goes first
   who_goes_first(CmptrGameScr, HmnGameScr, FirstPlayer),

   generate_empty_melds_storage(CmptrMelds),
   generate_empty_melds_storage(HmnMelds),

   round_loop(CmptrGameScr,HmnGameScr,0,0,RoundNumber,Stock,TrmpCrd,TrumpSuit,FirstPlayer,CmptrHnd,HmnHnd,[],[],CmptrMelds,HmnMelds,CmptrRoundScr,HmnRoundScr).


/* *********************************************************************
Rule Name: distribute_cards/4
Purpose: Distributes 12 cards to each player, 4 cards at a time
Parameters:
   Input:
      Deck: the deck for the round of pinochle
   Output: 
      NewDeck: the deck after cards have been distributed
      ComputerHand: The computer's hand after distribution
      HumanHand: The human's hand after distribution
Assistance Received: none
********************************************************************* */
distribute_cards(Deck, NewDeck, ComputerHand, HumanHand) :-
   distribute_cards(Deck, [], [], NewDeck, ComputerHand, HumanHand),
   write("Done distributing cards."), nl, nl.

distribute_cards(Deck, OldComputerHand, OldHumanHand, Deck, OldComputerHand, OldHumanHand) :-
   length(OldComputerHand, ComputerHandSize),
   ComputerHandSize >= 12,
   length(OldHumanHand, HumanHandSize),
   HumanHandSize >= 12.
   
distribute_cards(Deck, OldComputerHand, OldHumanHand, NewDeck, NewComputerHand, NewHumanHand) :-
   write("Giving human four cards..."),nl,
   give_cards_from_deck(4, Deck, OldHumanHand, UpdatedDeck1, IntermediateHumanHand),
   write("Human hand so far: "),
   get_card_pile_string(IntermediateHumanHand, HumanHandString),
   write(HumanHandString), nl, nl,

   write("Giving computer four cards..."),nl,
   give_cards_from_deck(4, UpdatedDeck1, OldComputerHand, UpdatedDeck2, IntermediateComputerHand),
   write("Computer hand so far: "),
   get_card_pile_string(IntermediateComputerHand, ComputerHandString),
   write(ComputerHandString), nl, nl,

   distribute_cards(UpdatedDeck2, IntermediateComputerHand, IntermediateHumanHand, NewDeck, NewComputerHand, NewHumanHand).

   
/* *********************************************************************
Rule Name: give_cards_from_deck/5
Purpose: Gives the specified numberof cards from the deck to the player to add to their hand
Parameters:
   Input:
      NumOfCards: The number of cards to give to the player
      OldDeck: the deck from which to give the cards to the player
      OldHand: THe hand of the player before giving them the cards from the deck
   Output: 
      NewDeck: the deck after cards have been given to the player
      NewHand: THe hand after cards have been given to the player
Assistance Received: none
********************************************************************* */
give_cards_from_deck(NumOfCards, OldDeck, OldHand, OldDeck, OldHand) :-
   NumOfCards =< 0.

give_cards_from_deck(NumOfCards, OldDeck, OldHand, NewDeck, NewHand) :-
   take_card_from_top(OldDeck, IntermediateDeck, CardAtTop),
   put_card_at_end_of_pile(CardAtTop, OldHand, IntermediateHand),
   NewNumOfCards is NumOfCards - 1,
   give_cards_from_deck(NewNumOfCards, IntermediateDeck, IntermediateHand, NewDeck, NewHand).



/* *********************************************************************
Rule Name: round_loop/17
Purpose: The loop that determines the 
Parameters:
   Input:
      CmptrGameScr: The current computer game score 
      HmnGameScr: The current human game score 
      CmptrRndScr: The current computer round score
      HmnRndScr: The current human round score
      RoundNumber: The current round number
      Stock: The current stock
      TrmpCrd: The trump card for the round
      TrumpSuit: The trump suit for the round
      NxtPlyr: The next player to play in the round
      CmptrHnd: The computer's current hand in the round
      HmnHnd: The human's current hand in the round
      CmptrCptrPile: The computer's current capture pile in the round
      HmnCptrPile: The human's current capture pile in the round
      CmptrMelds: The computer's current melds in the round
      HmnMelds: The human's current melds in the round
   Output: 
      NewCmptrRndScr: The score obtained by the computer player at the end of the round
      NewHmnRndScr:
Assistance Received: none
********************************************************************* */
round_loop(_,_,ComputerRoundScore,HumanRoundScore,_,_,_,_,_,ComputerHand,HumanHand,_,_,_,_,NewComputerRoundScore,NewHumanRoundScore) :-
   %if both players run out of cards in their hand, end the round
   ComputerHand = [],
   HumanHand = [],
   NewComputerRoundScore = ComputerRoundScore,
   NewHumanRoundScore = HumanRoundScore,
   write("Both players have run out of cards to play"),nl.
   

round_loop(
            %input arguments
            CmptrGameScr, 
            HmnGameScr, 
            CmptrRndScr,
            HmnRndScr,
            RoundNumber, 
            Stock, 
            TrmpCrd,
            TrumpSuit,
            NxtPlyr,
            CmptrHnd, 
            HmnHnd, 
            CmptrCptrPile, 
            HmnCptrPile,
            CmptrMelds,
            HmnMelds,
            %output arguments
            NewCmptrRndScr,
            NewHmnRndScr
         ) :-
   display_round_table(CmptrGameScr, HmnGameScr, CmptrRndScr, HmnRndScr, RoundNumber, Stock, TrmpCrd, NxtPlyr, CmptrHnd, HmnHnd, CmptrCptrPile, HmnCptrPile, CmptrMelds, HmnMelds),
   prompt_next_move(NxtPlyr, NextMove),
   make_next_move(NxtPlyr, NextMove, HmnHnd, HmnMelds, TrumpSuit, -1),
   play_lead_card(NxtPlyr, TrumpSuit, CmptrHnd, CmptrMelds, HmnHnd, UpdatedCmptrHnd1, UpdatedHmnHnd1, LeadCard),

   %it is now the other players turn to make a move
   switch_next_player(NxtPlyr, NewNxtPlyr),
   prompt_next_move(NewNxtPlyr, NewNextMove),
   make_next_move(NewNxtPlyr, NewNextMove, HmnHnd, HmnMelds, TrumpSuit, LeadCard),
   play_chase_card(NewNxtPlyr, LeadCard, TrumpSuit, UpdatedCmptrHnd1, CmptrMelds, UpdatedHmnHnd1, UpdatedCmptrHnd2, UpdatedHmnHnd2, ChaseCard),

   %find out winner of round and award points
   lead_vs_chase_card(LeadCard, ChaseCard, TrumpSuit, TurnResult),
   decide_turn_winner(NewNxtPlyr, TurnResult, Winner),
   declare_winner_and_award_points(Winner, TurnResult, LeadCard, ChaseCard, CmptrCptrPile, HmnCptrPile, PntsForHmn, PntsForCmptr, UpdatedCmptrCptrPile, UpdatedHmnCptrPile),

   %winner plays meld 
   play_meld(Winner, TrumpSuit, UpdatedCmptrHnd2, CmptrMelds, UpdatedHmnHnd2, HmnMelds, UpdatedCmptrMelds, UpdatedHmnMelds, MeldPntsForCmptr, MeldPntsForHmn),

   UpdatedCmptrRndScore is CmptrRndScr + PntsForCmptr + MeldPntsForCmptr,
   UpdatedHmnRndScore is HmnRndScr + PntsForHmn + MeldPntsForHmn,

   pick_one_card_each_from_stock(Winner, Stock, TrmpCrd, UpdatedCmptrHnd2, UpdatedHmnHnd2, UpdatedStock, UpdatedTrmpCrd, UpdatedCmptrHnd3, UpdatedHmnHnd3),

   prompt_save_game(
      CmptrGameScr, 
      HmnGameScr, 
      UpdatedCmptrRndScore,
      UpdatedHmnRndScore,
      RoundNumber, 
      UpdatedStock, 
      UpdatedTrmpCrd,
      Winner,
      UpdatedCmptrHnd3, 
      UpdatedHmnHnd3, 
      UpdatedCmptrCptrPile, 
      UpdatedHmnCptrPile,
      UpdatedCmptrMelds,
      UpdatedHmnMelds
   ),

   round_loop(CmptrGameScr, 
            HmnGameScr, 
            UpdatedCmptrRndScore,
            UpdatedHmnRndScore,
            RoundNumber, 
            UpdatedStock, 
            UpdatedTrmpCrd,
            TrumpSuit,
            Winner,
            UpdatedCmptrHnd3, 
            UpdatedHmnHnd3, 
            UpdatedCmptrCptrPile, 
            UpdatedHmnCptrPile,
            UpdatedCmptrMelds,
            UpdatedHmnMelds,
            NewCmptrRndScr,
            NewHmnRndScr
         ).




/* *********************************************************************
Rule Name: lead_vs_chase_card/4
Purpose: Determines whether the lead card or a chase card wins in a turn 
Parameters:
   Input:
      LeadCard: The lead card played in the turn
      ChaseCard: The chase card played in the turn
      TrumpSuit: The trump suit of the round
   Output: 
      Winner: Declared wether the lead card or the chase card wins
Assistance Received: none
********************************************************************* */
lead_vs_chase_card(LeadCard, ChaseCard, _, chase_card_wins) :-
   get_card_suit(LeadCard, LeadCardSuit),
   get_card_suit(ChaseCard, ChaseCardSuit),
   %if both cards have the same suit and the chase card has higher rank, chase card wins
   LeadCardSuit = ChaseCardSuit,
   card_with_higher_rank(LeadCard, ChaseCard, ChaseCard).

lead_vs_chase_card(LeadCard, ChaseCard, _, lead_card_wins) :-
   get_card_suit(LeadCard, LeadCardSuit),
   get_card_suit(ChaseCard, ChaseCardSuit),
   %lead card wins if both cards have the same suit and chase card does not have higher rank
   LeadCardSuit = ChaseCardSuit.

lead_vs_chase_card(_, ChaseCard, TrumpSuit, chase_card_wins) :-
   %chase card wins if it is of trump suit and lead card is not
   get_card_suit(ChaseCard, ChaseCardSuit),
   ChaseCardSuit = TrumpSuit.
   
lead_vs_chase_card(_, ChaseCard, TrumpSuit, lead_card_wins) :-
   %lead card wins if it is of trump suit and chase card is not
   get_card_suit(ChaseCard, ChaseCardSuit),
   ChaseCardSuit \= TrumpSuit.


/* *********************************************************************
Rule Name: decide_turn_winner/3
Purpose: Determines th winner of the turn
Parameters:
   Input:
      ChaseCardPlayer: The chase player of the turn
      WinningCard: Atom representing Whether the lead or chase card wins the turn
   Output: 
      Winner: The winner of the turn
Assistance Received: none
********************************************************************* */
decide_turn_winner(human, chase_card_wins, human).
decide_turn_winner(human, lead_card_wins, computer).
decide_turn_winner(computer, chase_card_wins, computer).
decide_turn_winner(computer, lead_card_wins, human).


/* *********************************************************************
Rule Name: declare_winner_and_award_points/10
Purpose: Declares the winner of the turn and awards them points
Parameters:
   Input:
      Winner: The play who won the turn
      WinningCard: Atom representing Whether the lead or chase card wins the turn
      LeadCard: The lead card of the turn
      ChaseCard: THe chase card of the turn
      CmptrCptrPile: The computer's capture pile
      HmnCptrPile: THe human's capture pile
   Output: 
      PntsForHmn: The points won by the human player
      PntsForCmptr: The points won by the computer player
      NewCmptrCptrPile: The updated computer capture pile
      NewHmnCptrPile: THe updated human capture pile
Assistance Received: none
********************************************************************* */
declare_winner_and_award_points(human, lead_card_wins, LeadCard, ChaseCard, CmptrCptrPile, HmnCptrPile, PntsForHmn, PntsForCmptr, CmptrCptrPile, NewHmnCptrPile) :-
   write("Your lead card has beaten the computer's chase card. You win this turn!"), nl,
   award_points_to_winner(human, LeadCard, ChaseCard, PntsForHmn, PntsForCmptr),
   write("You take both cards for your capture pile."), nl,
   NewHmnCptrPile = [LeadCard, ChaseCard | HmnCptrPile].


declare_winner_and_award_points(human, chase_card_wins, LeadCard, ChaseCard, CmptrCptrPile, HmnCptrPile, PntsForHmn, PntsForCmptr, CmptrCptrPile, NewHmnCptrPile) :-
   write("Your chase card has beaten the computer's lead card. You win this turn!"), nl,
   award_points_to_winner(human, LeadCard, ChaseCard, PntsForHmn, PntsForCmptr),
   write("You take both cards for your capture pile."), nl,
   NewHmnCptrPile = [LeadCard, ChaseCard | HmnCptrPile].


declare_winner_and_award_points(computer, lead_card_wins, LeadCard, ChaseCard, CmptrCptrPile, HmnCptrPile, PntsForHmn, PntsForCmptr, NewCmptrCptrPile, HmnCptrPile) :-
   write("The computer's lead card has beaten your chase card. The computer wins this turn."), nl,
   award_points_to_winner(computer, LeadCard, ChaseCard, PntsForHmn, PntsForCmptr),
   write("The computer takes both cards for its capture pile."), nl,
   NewCmptrCptrPile = [LeadCard, ChaseCard | CmptrCptrPile].


declare_winner_and_award_points(computer, chase_card_wins, LeadCard, ChaseCard, CmptrCptrPile, HmnCptrPile, PntsForHmn, PntsForCmptr, NewCmptrCptrPile, HmnCptrPile) :-
   write("The computer's chase card has beaten your lead card. The computer wins this turn."), nl,
   award_points_to_winner(computer, LeadCard, ChaseCard, PntsForHmn, PntsForCmptr),
   write("The computer takes both cards for its capture pile."), nl,
   NewCmptrCptrPile = [LeadCard, ChaseCard | CmptrCptrPile].



/* *********************************************************************
Rule Name: award_points_to_winner/5
Purpose: Generates and declares the points to give to the winner of the turn
Parameters:
   Input:
      Winner: The play who won the turn
      LeadCard: The lead card of the turn
      ChaseCard: THe chase card of the turn
   Output: 
      PntsForHmn: The points won by the human player
      PntsForCmptr: The points won by the computer player
Assistance Received: none
********************************************************************* */
award_points_to_winner(human, LeadCard, ChaseCard, PointsForHuman, 0) :-
   get_turn_points(LeadCard, ChaseCard, PointsForHuman),
   format("You won ~w points.~n~n", [PointsForHuman]).

award_points_to_winner(computer, LeadCard, ChaseCard, 0, PointsForComputer) :-
   get_turn_points(LeadCard, ChaseCard, PointsForComputer),
   format("The computer won ~w points.~n~n", [PointsForComputer]).


/* *********************************************************************
Rule Name: get_turn_points/3
Purpose: Generates the points won through lead and chase cards in a turn
Parameters:
   Input:
      LeadCard: The lead card of the turn
      ChaseCard: THe chase card of the turn
   Output: 
      TurnPoints: The points won through the lead and chase card
Assistance Received: none
********************************************************************* */
get_turn_points(LeadCard, ChaseCard, TurnPoints) :-
   card_points(LeadCard, LeadCardPoints),
   card_points(ChaseCard, ChaseCardPoints),
   TurnPoints is LeadCardPoints + ChaseCardPoints.


/* *********************************************************************
Rule Name: pick_one_card_each_from_stock/9
Purpose: Makes the players pick one card each from the stock
Parameters:
   Input:
      Winner: The winner of the turn that just ended
      Stock: The stock
      TrmpCrd: The trump card for the round
      CmptrHnd: The computer's hand
      HmnHnd: THe human's hand
   Output: 
      UpdatedStock: The stock after each player picks a card
      UpdatedTrmpCrd: The updated trump card (if the trump card is picked by one of the players)
      UpdatedCmptrHnd: The computer's hand after picking up the card
      UpdatedHmnHnd: The human's hand after picking up the card
Assistance Received: none
********************************************************************* */
pick_one_card_each_from_stock(_, [], TrmpCrd, CmptrHnd, HmnHnd, [], TrmpCrd, CmptrHnd, HmnHnd) :-
   write("No more cards left in stock. Play until cards in hand are exhausted."), nl, nl.

pick_one_card_each_from_stock(human, Stock, TrmpCrd, CmptrHnd, HmnHnd, UpdatedStock, UpdatedTrmpCrd, UpdatedCmptrHnd, UpdatedHmnHnd) :-
   length(Stock, StockSize),
   StockSize = 1,
   write("You pick a card from the stock first."),nl,
   [CardForHuman | UpdatedStock ] = Stock,
   write("The stock is out of cards."), nl,
   write("The computer picks up the trump card."),nl, nl,
   CardForComputer = TrmpCrd,
   get_card_suit(TrmpCrd, UpdatedTrmpCrd),
   put_card_at_end_of_pile(CardForHuman, HmnHnd, UpdatedHmnHnd),
   put_card_at_end_of_pile(CardForComputer, CmptrHnd, UpdatedCmptrHnd).

pick_one_card_each_from_stock(computer, Stock, TrmpCrd, CmptrHnd, HmnHnd, UpdatedStock, UpdatedTrmpCrd, UpdatedCmptrHnd, UpdatedHmnHnd) :-
   length(Stock, StockSize),
   StockSize = 1,
   write("The computer picks a card from the stock first."),nl,
   [CardForComputer| UpdatedStock ] = Stock,
   write("The stock is out of cards."), nl,
   write("You pick up the trump card."),nl, nl,
   CardForHuman = TrmpCrd,
   get_card_suit(TrmpCrd, UpdatedTrmpCrd),
   put_card_at_end_of_pile(CardForHuman, HmnHnd, UpdatedHmnHnd),
   put_card_at_end_of_pile(CardForComputer, CmptrHnd, UpdatedCmptrHnd).


pick_one_card_each_from_stock(human, Stock, TrmpCrd, CmptrHnd, HmnHnd, UpdatedStock, TrmpCrd, UpdatedCmptrHnd, UpdatedHmnHnd) :-
   write("You pick a card from the stock first."), nl,
   [CardForHuman | NewStock] = Stock,
   write("Then, the computer picks a card from the stock."), nl, nl,
   [CardForComputer | UpdatedStock] = NewStock,
   put_card_at_end_of_pile(CardForHuman, HmnHnd, UpdatedHmnHnd),
   put_card_at_end_of_pile(CardForComputer, CmptrHnd, UpdatedCmptrHnd).

pick_one_card_each_from_stock(computer, Stock, TrmpCrd, CmptrHnd, HmnHnd, UpdatedStock, TrmpCrd, UpdatedCmptrHnd, UpdatedHmnHnd) :-
   write("The computer picks a card from the stock first."), nl,
   [CardForComputer| NewStock] = Stock,
   write("Then you pick a card from the stock."), nl, nl,
   [CardForHuman | UpdatedStock] = NewStock,
   put_card_at_end_of_pile(CardForHuman, HmnHnd, UpdatedHmnHnd),
   put_card_at_end_of_pile(CardForComputer, CmptrHnd, UpdatedCmptrHnd).



/* *********************************************************************
Rule Name: display_round_table/14
Purpose: Displays the current table for the round
Parameters:
   Input:
      CmptrGameScr: The computer's current game score
      HmnGameScr: The human's current game score
      CmptrRoundScr: The computer's current round score
      HmnRoundScr:The human's current round score
      RoundNumber: The current round number
      Stock: The current stock 
      TrmpCrd: THe trump card for the round
      NxtPlyr: THe player who goes next
      CmptrHnd: The computer's hand
      HmnHnd: THe human's hand
      CmptrCptrPile: THe computer's capture pile
      HmnCptrPile: The human's capture pile
      CmptrMelds: The computer's melds
      HmnMelds: The human's melds
   Output: 
Assistance Received: none
********************************************************************* */
display_round_table(
                     CmptrGameScr, 
                     HmnGameScr, 
                     CmptrRoundScr,
                     HmnRoundScr,
                     RoundNumber, 
                     Stock,
                     TrmpCrd,
                     NxtPlyr,
                     CmptrHnd, 
                     HmnHnd, 
                     CmptrCptrPile, 
                     HmnCptrPile,
                     CmptrMelds,
                     HmnMelds
                  ) :-
   nl, nl, write("---------------------------------------------------------------------------------------"), nl,
   write("Current Round Table:"), nl, nl,
   %display current round
   format("Round: ~w~n~n", [RoundNumber]),

   %display each players state
   display_player_state(computer, CmptrGameScr, CmptrRoundScr, CmptrHnd, CmptrCptrPile, CmptrMelds), nl,
   display_player_state(human, HmnGameScr, HmnRoundScr, HmnHnd, HmnCptrPile, HmnMelds), nl,

   %display trump card
   card_or_suit_string(TrmpCrd, TrmpCrdString),
   format("Trump Card: ~s~n", [TrmpCrdString]),

   %display stock 
   get_card_pile_string(Stock, StockString),
   format("Stock: ~s~n~n", [StockString]),

   %display next player
   get_player_string(NxtPlyr, NextPlayerString),
   format("Next Player: ~s~n", [NextPlayerString]),
   write("---------------------------------------------------------------------------------------"), nl, nl.


/* *********************************************************************
Rule Name: display_player_state/14
Purpose: Displays the game score, round score, hand, capture pile, and melds of a player in a turn
Parameters:
   Input:
      Player: THe player whose round state is being displayed
      GameScore: The player's current game score
      RoundScore: The player's current round score
      Hand: The player's hand
      CapturePile: THe player's capture pile
      Melds: The player's melds
   Output: 
Assistance Received: none
********************************************************************* */
display_player_state(Player, GameScore, RoundScore, Hand, CapturePile, Melds) :-
   get_player_string(Player, PlayerString),
   get_card_pile_string_with_positions(Hand, HandString),
   get_card_pile_string(CapturePile, CapturePileString),
   get_melds_string(Melds, Hand, MeldsString),

   write(PlayerString), nl,
   format("    Score: ~w / ~w~n", [GameScore, RoundScore]),
   format("    Hand: ~s~n", [HandString]),
   format("    Capture Pile: ~s~n", [CapturePileString]),
   format("    Melds: ~s~n", [MeldsString]).
   

/* *********************************************************************
Rule Name: prompt_next_move/2
Purpose: Prompts the user to make the next move
Parameters:
   Input:
      WhoseTurn: The player whose turn it is to play
   Output: 
      Decision: The decision made by the user
Assistance Received: none
********************************************************************* */
prompt_next_move(computer, Decision) :-
   write("It is the computer's turn to play a card. What would you like to do?"),nl,
   write("Pick an action: "),nl,
   write("1.  Ask computer to make a move"), nl,
   write("2.  Quit the game"), nl, nl,
   write("Enter [1] or [2]: "),nl,

   read_string(user_input, "\n", "\r", _, StringInput),

   %convert string to atom
   atom_string(SelectedOption, StringInput),

   next_move_decision(computer, SelectedOption, Decision).

prompt_next_move(human, Decision) :-
   write("It is your turn to play a card. What would you like to do?"),nl,
   write("Pick an action: "),nl,
   write("1.  Make a move"), nl,
   write("2.  Ask for help"), nl,
   write("3.  Quit the game"), nl, nl,
   write("Enter [1], [2], or [3]: "),nl,

   read_string(user_input, "\n", "\r", _, StringInput),
   %convert string to atom
   atom_string(SelectedOption, StringInput),

   next_move_decision(human, SelectedOption, Decision).




/* *********************************************************************
Rule Name: next_move_decision/3
Purpose: Generates what decision the user made regarding the next move depending on his/her input
Parameters:
   Input:
      WhoseTurn: The player whose turn it is to play
      UserInput: The input by the user
   Output: 
      Decision: The decision made by the user, in its atomic representation
Assistance Received: none
********************************************************************* */
next_move_decision(_, '1', make_move).

next_move_decision(computer, '2', quit_game).

next_move_decision(human, '2', ask_help).

next_move_decision(human, '3', quit_game).

next_move_decision(computer, _, Decision) :-
   write("Invalid input. You must enter either [1] or [2]. Please try again."), nl,
   prompt_next_move(computer, Decision).

next_move_decision(human, _, Decision) :-
   write("Invalid input. You must enter either [1], [2], or [3]. Please try again."), nl,
   prompt_next_move(human, Decision).



/* *********************************************************************
Rule Name: make_next_move/3
Purpose: Proceed to the next step in the game depending on what move the user decided to do
Parameters:
   Input:
      WhoseTurn: The player whose turn it is to play
      Deicision: The decision made by the user, in its atomic representation
      Hand: The player's hand
      Melds: The player's melds
      TrumpSuit: the trump suit for the round
      LeadCard: The lead card for the round (-1 if the lead card has not been played yet)
   Output: 
Assistance Received: none
********************************************************************* */
make_next_move(_, make_move, _, _, _, _) :-
   %do nothing, so that the round loops proceeds unto the step where a player actually makes a move
   true.

make_next_move(human, ask_help, HumanHand, HumanMelds, TrumpSuit, -1) :-
   %human asked for help with lead card
   %decide what card to play as lead card
   suggest_lead_card(HumanHand, HumanMelds, TrumpSuit, LeadCard, Logic),

   %display hint with reasoning
   card_string(LeadCard, LeadCardString),
   get_card_position_in_pile(HumanHand, LeadCard, Position),
   format("~nIt is recommended that you play ~s(~w) as your lead card because ~s.~n~n", [LeadCardString, Position, Logic]).

make_next_move(human, ask_help, HumanHand, HumanMelds, TrumpSuit, LeadCard) :-
   %human asked for help with chase card
   %decide what card to play as chase card
   suggest_chase_card(HumanHand, HumanMelds, TrumpSuit, LeadCard, ChaseCard, Logic),

   %display hint with reasoning
   card_string(ChaseCard, ChaseCardString),
   get_card_position_in_pile(HumanHand, ChaseCard, Position),
   format("~nIt is recommended that you play ~s(~w) as your chase card because ~s.~n~n", [ChaseCardString, Position, Logic]).

make_next_move(_, quit_game, _, _, _, _) :-
   write("Thank you for playing Pinochle! Exiting game..."),
   halt(0).




/* *********************************************************************
Rule Name: who_goes_first/3
Purpose: Determines which player goes first at the beginning of a new round
Parameters:
   Input:
      ComputerGameScore: the computer's game score
      HumanGameScore: The human's game score
   Output: 
      FirstPlayer: the player to go first
Assistance Received: none
********************************************************************* */
who_goes_first(ComputerGameScore, HumanGameScore, computer) :-
   ComputerGameScore > HumanGameScore.

who_goes_first(ComputerGameScore, HumanGameScore, human) :-
   HumanGameScore > ComputerGameScore.

who_goes_first(ComputerGameScore, HumanGameScore, FirstPlayer) :-
   ComputerGameScore = HumanGameScore,
   toss_coin(FirstPlayer).


/* *********************************************************************
Rule Name: toss_coin/1
Purpose: Tosses a coin to determine which player goes first
Parameters:
   Input:
   Output: 
      FirstPlayer: the player to go first
Assistance Received: none
********************************************************************* */
toss_coin(FirstPlayer) :- 
   write("Deciding who goes first based on a coin toss. Enter your prediction (heads/tails): "), nl,
   read_string(user_input, "\n", "\r", _, StringInput),
   %convert string to atom
   atom_string(Prediction, StringInput),
   coin_toss_result(TossResult),
   decide_coin_toss_winner(Prediction, TossResult, FirstPlayer),
   format("The toss resulted in a ~w.~n", [TossResult]),
   format("The ~w player goes first.~n", [FirstPlayer]).
   
/* *********************************************************************
Rule Name: coin_toss_result/1
Purpose: Generates the result of tossing a coin
Parameters:
   Input:
   Output: 
      TossResult: the face of the coin that is tossed
Assistance Received: none
********************************************************************* */
coin_toss_result(TossResult) :-
   random_between(0, 1, IntResult),
   int_to_coin_face(IntResult, TossResult).


/* *********************************************************************
Rule Name: int_to_coin_face/2
Purpose: Translates integer to coin face
Parameters:
   Input:
      Int: the integer representation of the coin face
   Output: 
      TossResult: the face of the coin corresponding to the given integer
Assistance Received: none
********************************************************************* */
int_to_coin_face(0, heads).

int_to_coin_face(1, tails).


/* *********************************************************************
Rule Name: decide_coin_toss_winner/3
Purpose: Determines the winner of a coin toss 
Parameters:
   Input:
      Prediction: The human player's prediction for the coin toss
      TossResult: the face of the coin corresponding to the given integer
   Output: 
      FirstPlayer: the player to go first
Assistance Received: none
********************************************************************* */
decide_coin_toss_winner(heads, TossResult, human) :-
   TossResult = heads.

decide_coin_toss_winner(heads, _, computer).

decide_coin_toss_winner(tails, TossResult, human) :-
   TossResult = tails.

decide_coin_toss_winner(tails, _, computer).

decide_coin_toss_winner(_, TossResult, FirstPlayer) :-
   write("You must enter either 'heads' or 'tails'. Please try again:"), nl,
   read_string(user_input, "\n", "\r", _, StringInput),
   %convert string to atom
   atom_string(Prediction, StringInput),
   decide_coin_toss_winner(Prediction, TossResult, FirstPlayer).



/* *********************************************************************
Rule Name: switch_next_player/2
Purpose: Given a player, returns the other player in the game
Parameters:
   Input:
      PreviousPlayer: The previous player to make a move
   Output: 
      NextPlayer: the next player to make a move
Assistance Received: none
********************************************************************* */
switch_next_player(computer, human).
switch_next_player(human, computer).



