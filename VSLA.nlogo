extensions [bitmap csv]

breed [borders border]
breed [visuals visual]
breed [activities activity]
breed [shocks shock]
breed [selections selection]
breed [arrows arrow]
breed [plusses plus]
breed [minuses minus]
breed [scorecards scorecard]
breed [panel-selections panel-selection]
breed [score-labels score-label]
breed [farm-actions farm-action]
breed [farm-shocks farm-shock]
breed [farm-summaries farm-summary]
breed [integer-agents integer-agent]
breed [buttons button]

patches-own [inGame?]
selections-own [row column counter visibleTo showing]
plusses-own [counter visibleTo showing]
minuses-own [counter visibleTo showing]
shocks-own [ID shock_name image pAffect ind? target_act_var pLose amount phase_occur]
activities-own [ID	activity_name image	req	amt_min	amt_max annual badge phase_shown cost benefit summary supply_per payment currentSupply adds_to takes_from negates initial]
arrows-own [identity menu at-end]
scorecards-own [identity]
panel-selections-own [ID visibleTo counter]
farm-actions-own [ID visibleTo counter]
farm-shocks-own [ID visibleTo counter]
farm-summaries-own [ID summaryOf counter]
integer-agents-own [ID visibleTo summaryOf]
buttons-own [identity]


globals [
  ;;game mechanism values
  numPlayers
  playerNames
  playerPosition
  playerCounts
  messageAddressed
  gameInProgress
  langSuffix
  numPatches
  playerActivities
  playerConfirm
  playerConfirmShock
  currentYear
  currentPhase
  playerCurrentSelections
  playerAccess
  playerAccessBySupply
  playerResources
  playerCurrentResources
  playerPendingBenefits
  playerConstrainedSupplies
  playerEarnedPayments

  ;;visualization parameters and variables
  colorList
  rgb_R
  rgb_G
  rgb_B
  border_color
  playerColor
  arrowEndColor
  arrowNotEndColor
  playerSummaryXLoc
  player_width_inner
  player_width_outer
  number_shape_list

  confirmFracLoc
  confirmPixLoc
  confirmPatchLoc
  confirmShockPatchLoc
  confirmTile
  confirm-up-color
  confirm-down-color


  x_pixels
  y_pixels
  x_panel_pixels
  pixelsPerPatch
  patches_panel
  visualization_at_start
  other_player_summary_area
  arrow_area
  activities_per_row
  selection_rel_size
  plusminus_rel_size
  panel_selection_rel_size
  panel_all_rel_size
  panel_selections_y
  shock_selection_rel_size
  shock_all_rel_size
  shock_losses_y
  shock_images_y
  payment_actions_y
  payment_images_y
  farm_actions_y
  farm_own_summary_y
  farm_summary_y
  shock_canvas_size

  ;;variables related to parsing parameter input
  activityListFileName
  functionListFileName
  endFunctionListFileName
  shockListFileName
  nonplayerFunctionListFileName
  gameName
  inputFileLabels
  completedGamesIDs
  parsedInput
  currentSessionParameters
  gameTag
  sessionList
  shockList
  shockInputs
  shock_property_labels
  activityList
  activityInputs
  activity_property_labels

  nonplayerFunctionList
  nonplayerFunctionList_String
  nonplayerFunctionInputs
  nonplayerFunctionNames
  nonplayerFunctionActivities
  nonplayerFunctionPhases
  nonplayerFunction_property_labels

  endFunctionList
  endFunctionList_String
  endFunctionInputs
  endFunctionNames
  endFunctionActivities
  endFunctionPhases
  endFunction_property_labels

  functionList
  functionList_String
  functionInputs
  functionNames
  functionActivities
  functionPhases
  function_property_labels
  parameterHandled
  tempFn
  tempList
  tempActivityList
  activityCostList
  activityBenefitList


  ;;parameters to be set by input file
  gameID
  years
  phases
  vsla_int_rate
  lender_int_rate
  endowment
  yield_low
  yield_high
  vsla_chest
  cash_transfer
  opening_vsla
  payoff_vsla
  count_shares
  first_endowment



  ;;for debugging all local variables
  tempLocal

  ]




;;EXAMPLES OF PROCEDURES THAT GAMES **WILL DEFINITELY** USE

to start-hubnet

  ;; clean the slate
  clear-all
  hubnet-reset

  ;; set all session variables that are preserved across games
  set playerNames (list)

  set playerPosition (list)
  set playerColor (list)
  set playerSummaryXLoc (list)
  set numPlayers 0
  set gameInProgress 0

  set-default-shape borders "line-thick"
  set-default-shape selections "circle"
  set-default-shape activities "blank"
  set-default-shape shocks "blank"
  set-default-shape arrows "activity-arrow"
  set-default-shape plusses "badge-plus"
  set-default-shape minuses "badge-minus"
  set-default-shape scorecards "blank"

  set number_shape_list (list "num-0" "num-1" "num-2" "num-3" "num-4" "num-5" "num-6" "num-7" "num-8" "num-9")

  set arrowEndColor gray
  set arrowNotEndColor blue


  set x_pixels 1080
  set y_pixels 720
  set x_panel_pixels 360
  set numPatches 8
  set other_player_summary_area numPatches / 4
  set arrow_area numPatches / 8
  set activities_per_row 3
  set selection_rel_size 0.9
  set plusminus_rel_size 0.3
  set panel_selection_rel_size 0.2
  set panel_all_rel_size 0.9
  set panel_selections_y numPatches * 0.75
  set shock_selection_rel_size 0.2
  set shock_all_rel_size 0.5
  set shock_losses_y numPatches * 0.5
  set shock_images_y numPatches * 0.7
  set payment_images_y numPatches * 0.4
  set payment_actions_y numPatches * 0.4
  set farm_actions_y numPatches * 0.3
  set farm_summary_y numPatches * 0.05
  set farm_own_summary_y numPatches * 0.1
  set shock_canvas_size numPatches * 0.7

  resize_activities

  set rgb_G 100
  set rgb_B 0
  set rgb_R 150
  set border_color 5
  set colorList (list 95 15 25 115 125 5 135 93 13 23 113 123 3 133 98 18 28 118 128 8 138)  ;; add to this if you will have more than 21 players, but really, you shouldn't!!!
  set confirm-up-color lime
  set confirm-down-color 61


  ;; clear anything from the display, just in case
  clear-ticks
  clear-patches
  clear-turtles
  clear-drawing

  ;; try to read in the input parameter data - stop if the file doesn't exist
  if not file-exists? inputParameterFileName [ ;;if game parameter file is incorrect
    user-message "Please enter valid file name for input data"
    stop
  ]

  ;; open the file and read it in line by line
  set parsedInput csv:from-file inputParameterFileName
  set inputFileLabels item 0 parsedInput
  set sessionList []
  foreach but-first parsedInput [ ?1 -> set sessionList lput (item 0 ?1) sessionList ]
  set sessionList remove-duplicates sessionList

  ;; look in the list of completed game IDs, and take an initial guess that the session of interest is one higher than the highest session completed previously
  set completedGamesIDs []
  ifelse file-exists? "completedGames.csv" [
  file-open "completedGames.csv"
  while [not file-at-end?] [
    let tempValue file-read-line
    set completedGamesIDs lput read-from-string substring tempValue 0 position "_" tempValue completedGamesIDs
  ]
  set completedGamesIDs remove-duplicates completedGamesIDs
  set sessionID max completedGamesIDs + 1
  file-close
  ] [
  set sessionID -9999
  ]


  set currentSessionParameters []

end


to initialize-session

  ;; stop if we are currently in a session
  if (length currentSessionParameters > 0)
  [user-message "Current session is not complete.  Please continue current session.  Otherwise, to start new session, please first clear settings by clicking 'Launch Broadcast'"
    stop]

  ;; if the session requested isn't in our input parameters, stop
  if (not member? sessionID sessionList)
  [user-message "Session ID not found in input records"
    stop]

  ;; if the session requested has prior game data available, let the user know
  if (member? sessionID completedGamesIDs)
  [user-message "Warning: At least one game file with this sessionID has been found"]

  ;; pick the appropriate set of parameters for the current session from the previously parsed input file (i.e., all games listed as part of session 1)
  set currentSessionParameters filter [ ?1 -> item 0 ?1 = sessionID ] parsedInput

end

to start-game

  ;; stop if a game is already running
  if (gameInProgress = 1)
  [user-message "Current game is not complete.  Please continue current game.  Otherwise, to start new session, please first clear settings by clicking 'Launch Broadcast'"
    stop]


  ;; stop if there are no more game parameters queued up
  if (length currentSessionParameters = 0)
  [user-message "No games left in session.  Re-initialize or choose new session ID"
    stop]

  ;; clear the output window and display
  clear-output
  clear-patches
  clear-turtles
  clear-drawing
  foreach playerPosition [ ?1 ->
    hubnet-clear-overrides (item (?1 - 1) playerNames)
  ]
  output-print "Output/Display cleared."

  ;; Start the game output file
  ;; the code below builds the game output file, named by the players and the current timestamp
  let tempDate date-and-time
  foreach [2 5 8 12 15 18 22] [ ?1 -> set tempDate replace-item ?1 tempDate "_" ]
  let playerNameList (word (item 0 playerNames) "_")
  foreach n-values (numPlayers - 1) [ ?1 ->  ?1 + 1 ] [ ?1 ->
   set playerNameList (word playerNameList "_" (item ?1 playerNames))
  ]
  set gameName (word gameTag "_" playerNameList "_" tempDate ".csv" )
  carefully [file-delete gameName file-open gameName] [file-open gameName]
  output-print "Game output file created."


  ;; read in game file input
  set-game-parameters

  ;; read in game activities
  read-activity-file
  read-function-file
  read-nonplayer-function-file
  read-end-function-file
  read-shock-file

  ;;Set any parameters not set earlier, and not to be set from the read-in game file
  set playerCounts n-values numPlayers [0]
  set playerResources n-values numPlayers [endowment]
  set playerCurrentResources n-values numPlayers [0]
  set playerPendingBenefits n-values numPlayers [0]

  ;; set up lists for number of each activities selected by player
  set playerActivities n-values numPlayers [0]
  set playerCurrentSelections n-values numPlayers [0]
  let playerActivityList n-values count activities [0]
  let playerSelectionList n-values count activities [0]
  ;; set any initial values for activities
  ask activities [
    set playerActivityList replace-item (ID - 1) playerActivityList initial
  ]

  foreach n-values (numPlayers) [?1 -> ?1] [ ?1 ->
    set playerActivities replace-item ?1 playerActivities playerActivityList
    set playercurrentSelections replace-item ?1 playerCurrentSelections playerSelectionList
  ]



  ;; have a list where 'supply' for activities can be stored



  set playerAccess playerActivities
  set playerAccessBySupply playerAccess
  set playerConstrainedSupplies playerAccess
  set playerEarnedPayments playerAccess


  ;set-default-shape borders "line"
  output-print "Game parameters initialized."





  ;;lay out the game board
  set pixelsPerPatch y_pixels / numPatches
  set patches_panel round(x_panel_pixels / pixelsPerPatch)
  resize-world (- patches_panel) (numPatches - 1) 0 (numPatches - 1)
  set-patch-size pixelsPerPatch ;;
  resize_activities


  ;; add player list at the bottom
  map_player_summaries

  ;; mark out board areas
  ;; patches_panel is the 'self' summary area
  ;; numPatches is the size of the game play area


  ;;show divisions between patches
  make-borders

  ;;add arrows
  let tempMenu n-values numPlayers [0]
  create-arrows 1 [setxy (numPatches - arrow_area / 2 - 0.5) (other_player_summary_area + (numPatches - other_player_summary_area) * 3 / 4 - 1) set heading 0 set size 1 set identity "up" set menu tempMenu set at-end tempMenu]
  create-arrows 1 [setxy (numPatches - arrow_area / 2 - 0.5) (other_player_summary_area + (numPatches - other_player_summary_area) * 1 / 4 ) set heading 180 set size 1 set identity "down" set menu tempMenu set at-end tempMenu]

  ;;and then do whatever else you'd do to initialize the game


  let yConvertPatch (numPatches / y_pixels)  ;;scaling vertical measures based on the currently optimized size of 50
  let xyConvertPatchPixel (patch-size / pixelsPerPatch)  ;; scaling vertical and horizontal measures based on currently optimized patch size of 14


  set confirmFracLoc (list 0.55 0.025 0.4 0.1) ;;fraction of height and width of panel, in form (list x-min y-min width height)
  set confirmPixLoc convertFracPix confirmFracLoc
  set confirmPatchLoc convertPixPatch confirmPixLoc
  create-buttons 1 [setxy ((item 0 confirmPatchLoc) + (item 2 confirmPatchLoc) / 2) ((item 1 confirmPatchLoc) - (item 3 confirmPatchLoc) / 2) set size (item 2 confirmPatchLoc) set color confirm-up-color set identity "confirm" set shape "confirm"]

  set confirmShockPatchLoc confirmPatchLoc
  set confirmShockPatchLoc replace-item 0 confirmShockPatchLoc (numPatches * 0.75)
  set confirmShockPatchLoc replace-item 1 confirmShockPatchLoc (other_player_summary_area + 0.5)
  create-buttons 1 [setxy ((item 0 confirmShockPatchLoc) + (item 2 confirmShockPatchLoc) / 2) ((item 1 confirmShockPatchLoc) - (item 3 confirmShockPatchLoc) / 2) set size (item 2 confirmShockPatchLoc) set color confirm-up-color set identity "confirm_shock" set shape "confirm" set hidden? true]



  ;;run any functions that should be run before the start of the game
  set currentPhase 0
  update-nonplayer-functions
  update-functions

  set playerConfirm n-values numPlayers [0]
  set playerConfirmShock playerConfirm
  set gameInProgress 1
  set currentYear 1
  set currentPhase 1



  update-current-supply
  update-takes-from


  foreach n-values numPlayers [?1 -> ?1] [ ?1 ->
    update-access ?1
    update-displayed-activities ?1
    update-farm-actions ?1
    update-current-selections ?1
  ]


   ;;add a round counter
  create-scorecards 1 [setxy (patches_panel * (- 0.85)) (numPatches - 0.92) set label (word currentYear " - " currentPhase) set identity "roundCounter"]

end

to listen

  ;; this is the main message processing procedure for the game.  this procedure responds to hubnet messages, one at a time, as long as the 'Listen Clients' button is down
  ;; where appropriate, this procedure can be made easier to read by exporting the response to the message case to its own procedure

  ;; while there are messages to be processed
  while [hubnet-message-waiting?] [

    ;; we use a 'message addressed' flag to avoid having to nest foreach loops (there is no switch/case structure in netlogo)
    ;; the procedure steps through each case from top to bottom until it finds criteria that fit.  it then executes that code and marks the message addressed
    ;; it is CRITICAL that you order the cases correctly - from MOST SPECIFIC to LEAST SPECIFIC - if there is any ambiguity in interpreting the message
    ;; e.g., where you have a button or other feature that sits within a larger clickable area, list the case for the button first, then the larger area surrounding it
    set messageAddressed 0

    ;; get the next message in the queue
    hubnet-fetch-message



    ;; CASE 1 and CASE 2 messages are responses to messages of players coming in and out of the game.  hopefully, what's there is a good fit to your purpose

    ;; CASE 3  are responses to a click/tap from a player.

    ;; CASE 4  are 'mouse up' responses after clicking, which at present we are not using.  these are helpful to make use of 'mouse drag' actions

    ;; CASE 5  are otherwise unhandled messages

    ;; CASE 1 if the message is that someone has entered the game
    if (hubnet-enter-message? and messageAddressed = 0)[

      ;; CASE 1.1 if the player has already been in the game, link them back in.  if the player is new, set them up to join
      ifelse (member? hubnet-message-source playerNames ) [

        ;; pre-existing player whose connection cut out
        let newMessage word hubnet-message-source " is back."
        hubnet-broadcast-message newMessage

        ;; give the player the current game information
        let currentMessagePosition (position hubnet-message-source playerNames);
        let currentPlayer currentMessagePosition + 1
        send-game-info currentMessagePosition
        update-displayed-activities currentMessagePosition
        update-current-supply
        update-takes-from


      ] ;; end previous player re-entering code

      [ ;; CASE 1.2 otherwise it's a new player trying to join

        ;; new players can only get registered if a game isn't underway
        if (gameInProgress = 0) [  ;;only let people join if we are between games


          ;; register the player name
          set playerNames lput hubnet-message-source playerNames


          ;; add the new player, and give them a color
            set numPlayers numPlayers + 1
          set playerPosition lput numPlayers playerPosition
          set playerColor lput (item (numPlayers - 1) colorList) playerColor

          ;; let everyone know
          let newMessage word hubnet-message-source " has joined the game."
          hubnet-broadcast-message newMessage
          ;file-print (word hubnet-message-source " has joined the game as Player " numPlayers " at " date-and-time)

        ]  ;; end new player code
      ] ;; end ifelse CASE 1.1 / CASE 1.2

      ;; mark this message as done
      set messageAddressed 1
    ] ;; end CASE 1

    ;; CASE 2 if the message is that someone left
    if (hubnet-exit-message? and messageAddressed = 0)[

      ;; nothing to do but let people know
      let newMessage word hubnet-message-source " has left.  Waiting."
      hubnet-broadcast-message newMessage

      ;; mark the message done
      set messageAddressed 1
    ] ;; end CASE 2

    ;;CASE 3 the remaining cases are messages that something has been tapped, which are only processed if 1) a game is underway, 2) the message hasn't been addressed earlier, and 3) the player is in the game
    if (gameInProgress = 1 and messageAddressed = 0 and (member? hubnet-message-source playerNames))[

      ;; identify the sender
      let currentMessagePosition (position hubnet-message-source playerNames);  ;;who the message is coming from, indexed from 0
      let currentPlayer (currentMessagePosition + 1); ;;who the player is, indexed from 1

      if (hubnet-message-tag = "View" )  [  ;; the current player tapped something in the view


        ;;identify the patch
        let xPatch (item 0 hubnet-message)
        let yPatch (item 1 hubnet-message)
        let xPixel (xPatch - min-pxcor + 0.5) * patch-size
        let yPixel (max-pycor + 0.5 - yPatch) * patch-size

        let currentPXCor [pxcor] of patch xPatch yPatch
        let currentPYCor [pycor] of patch xPatch yPatch

        let patchInGame [inGame?] of patch xPatch yPatch

        ;; CASE 3.1 if the tap is on the confirm button
        if ( clicked-area (confirmPixLoc) and messageAddressed = 0)[

          ;; mark the confirm and record
          let newMessage word (item (currentPlayer - 1) playerNames) " clicked confirm."
          hubnet-broadcast-message newMessage
          output-print newMessage
          file-print (word "Case 3.1 - Player " currentPlayer " clicked confirm button at " date-and-time)


          ;; DO WHATEVER SHOULD BE DONE TO THE APPEARANCE WHEN CONFIRM IS PRESSED
          ;; (COLOR CHANGE, DISAPPEARING THINGS, ETC.)

          process-confirm (currentPlayer - 1)

          ;; mark message done
          set messageAddressed 1
        ] ;; end 3.1

        ;; CASE 3.2 if the tap is on the confirm shock button
        if ( clicked-area (convertPatchPix confirmShockPatchLoc) and messageAddressed = 0)[

          ;; mark the confirm and record
          let newMessage word (item (currentPlayer - 1) playerNames) " clicked confirm on the shock info."
          hubnet-broadcast-message newMessage
          output-print newMessage
          file-print (word "Case 3.2 - Player " currentPlayer " clicked confirm shock button at " date-and-time)


          ;; DO WHATEVER SHOULD BE DONE TO THE APPEARANCE WHEN CONFIRM IS PRESSED
          ;; (COLOR CHANGE, DISAPPEARING THINGS, ETC.)

          process-confirm-shock (currentPlayer - 1)

          ;; mark message done
          set messageAddressed 1
        ] ;; end 3.2

        ;; CASE 3.2 if the tap is on a plus sign
        if messageAddressed = 0 and item (currentPlayer - 1) playerConfirm = 0 [
          ask plusses with [hidden? = false and item currentMessagePosition visibleTo = 1] [
           let plusLoc (list (xcor - size / 2) (ycor + size / 2) size size)
          if clicked-area ( convertPatchPix plusLoc) [


              ;; mark the confirm and record
              let newMessage word (item (currentPlayer - 1) playerNames) " clicked a plus."
              hubnet-broadcast-message newMessage
              output-print newMessage
              file-print (word "Case 3.2 - Player " currentPlayer " clicked a plus at " date-and-time)

              plus-activity currentMessagePosition



              ;; mark message done
              set messageAddressed 1
            ]

          ]
          update-current-selections currentMessagePosition
          update-current-supply
          update-takes-from

        ] ;; end 3.2


        ;; CASE 3.3 if the tap is on a minus sign
        if messageAddressed = 0 and item (currentPlayer - 1) playerConfirm = 0 [
         ask minuses with [hidden? = false and item currentMessagePosition visibleTo = 1][
            let minusLoc (list (xcor - size / 2) (ycor + size / 2) size size)

            if clicked-area ( convertPatchPix minusLoc) [

              ;; mark the confirm and record
              let newMessage word (item (currentPlayer - 1) playerNames) " clicked a minus."
              hubnet-broadcast-message newMessage
              output-print newMessage
              file-print (word "Case 3.3 - Player " currentPlayer " clicked a minus at " date-and-time)

              minus-activity currentMessagePosition


              ;; mark message done
              set messageAddressed 1

            ]

          ]
          update-current-selections currentMessagePosition
          update-current-supply
          update-takes-from

        ] ;; end 3.3


        ;; CASE 3.4 if the tap is on an arrow
        if messageAddressed = 0 and item (currentPlayer - 1) playerConfirm = 0 [
          ask arrows with [hidden? = false][
            let arrowLoc (list (xcor - size / 2) (ycor + size / 2) size size)

            if clicked-area ( convertPatchPix arrowLoc) [

              ;; mark the confirm and record
              let newMessage word (item (currentPlayer - 1) playerNames) " clicked an arrow."
              hubnet-broadcast-message newMessage
              output-print newMessage
              file-print (word "Case 3.3 - Player " currentPlayer " clicked an arrow at " date-and-time)

              ;; mark message done
              set messageAddressed 1
              arrow-activity currentMessagePosition identity (item (currentMessagePosition) menu)

            ]

          ]
        ] ;; end 3.4


      ] ;; end all cases for clicks within the  "view"


    ] ;; end all CASE 3 messages

    ;; CASE 4 - Mouse up after a click message
    if (gameInProgress = 1 and messageAddressed = 0 and hubnet-message-tag = "Mouse Up") [

     ;; no need to do anything
     set messageAddressed 1
    ]

    if (gameInProgress = 1 and messageAddressed = 0) [
      ;; CASE 5 if the message still hasn't been addressed, it means players clicked in a place that they weren't meant to - ignore it
      set messageAddressed 1
      output-print "Unhandled message"

    ]
  ]


end




;;EXAMPLES OF PROCEDURES THAT GAMES **WILL PROBABLY** USE


to-report convertFracPix [fracLoc]
  let pixLoc (list ((item 0 fracLoc) * x_panel_pixels) ((item 1 fracLoc) * y_pixels) ((item 2 fracLoc) * x_panel_pixels) ((item 3 fracLoc) * y_pixels))
  report pixLoc
end

to-report convertPixPatch [pixLoc]
  let patchLoc (list (min-pxcor - 0.5 + (item 0 pixLoc) / pixelsPerPatch) (max-pycor + 0.5 - (item 1 pixLoc) / pixelsPerPatch) ((item 2 pixLoc) / pixelsPerPatch) ((item 3 pixLoc) / pixelsPerPatch))
  report patchLoc
end

to-report convertPatchPix [patchLoc]
  let pixLoc (list (((item 0 patchLoc) - min-pxcor + 0.5) * pixelsPerPatch) (( max-pycor + 0.5 - (item 1 patchLoc)) * pixelsPerPatch) ((item 2 patchLoc) * pixelsPerPatch) ((item 3 patchLoc) * pixelsPerPatch))
  report pixLoc
end

to-report clicked-area [ currentPixLoc ]

  ;; checks the boundaries of a click message against those of a 'button' to see if it was the one clicked
  ;; inputs are boundaries in PIXEL SPACE


  let xPixel ((item 0 hubnet-message) - min-pxcor + 0.5) * patch-size
  let yPixel (max-pycor + 0.5 - (item 1 hubnet-message)) * patch-size

  let xPixMin item 0 currentPixLoc
  let xPixMax item 0 currentPixLoc + item 2 currentPixLoc
  let yPixMin item 1 currentPixLoc
  let yPixMax item 1 currentPixLoc + item 3 currentPixLoc
  ifelse xPixel > xPixMin and xPixel < xPixMax and yPixel > yPixMin and yPixel < yPixMax [  ;; player "clicked"  the current button
    report true
  ] [
    report false
  ]

end

to make-borders

  ;; use line and box drawing functions to map out boundaries necessary in this game


  draw_vert_line -0.5 -0.499 8 green 1
  draw_horiz_line -4.5 3 4 green 1


end




;;EXAMPLES OF PROCEDURES THAT GAMES **MIGHT** USE

to send-game-info [currentPosition]

  ;; sends current, player-specific game info to the specified player.  this is useful if the player has left the game and returned, so that any view overrides are re-established.
  ;;example:  ask dayBoxes [hubnet-send-override (item currentPosition playerNames) self "hidden?" [true]]

end


to resize_activities
  let x_fit (numPatches  - arrow_area) / activities_per_row
  let activity_rows  floor ((numPatches - other_player_summary_area) / x_fit)

  let y_fit x_fit

  ;;how much empty space do we need to account for
  let x_spacer (numPatches / activities_per_row - x_fit) / activities_per_row
  let y_spacer ((numPatches - other_player_summary_area) / activity_rows - y_fit) / activity_rows

  ;; now we know how many will fit in our space (activity_rows by activities_per_row
  ;; place them starting from the top
  let selectionCounter 0
  foreach n-values (activity_rows) [ ?1 ->  ?1 + 1 ] [ ?1 ->
    let row_num ?1
    foreach n-values (activities_per_row) [ ?2 ->  ?2 + 1 ] [ ?2 ->
      let col_num ?2
      set selectionCounter selectionCounter + 1
      create-selections 1 [setxy (x_fit + x_spacer) * (col_num - 1) + (x_fit + x_spacer) / 2 - 0.5 (max-pycor - (y_fit + y_spacer) * (row_num - 1) - (y_fit + y_spacer) / 2 + 0.5)
        set size x_fit * selection_rel_size
        set counter selectionCounter
        set visibleTo n-values numPlayers [0]
        set showing n-values numPlayers [0]
      ]

      create-plusses 1 [setxy (x_fit + x_spacer) * (col_num - 1) + (x_fit + x_spacer) / 4 - 0.5 (max-pycor - (y_fit + y_spacer) * (row_num - 1) - (y_fit + y_spacer) * 3 / 4 + 0.5 )
        set size x_fit * plusminus_rel_size
        set color gray
        set counter selectionCounter
        set visibleTo n-values numPlayers [0]
        set showing visibleTo
      ]
      create-minuses 1 [setxy (x_fit + x_spacer) * (col_num - 1) + (x_fit + x_spacer) * 3 / 4 - 0.5 (max-pycor - (y_fit + y_spacer) * (row_num - 1) - (y_fit + y_spacer) * 3 / 4 + 0.5 )
        set size x_fit * plusminus_rel_size
        set color gray
        set counter selectionCounter
        set visibleTo n-values numPlayers [0]
        set showing visibleTo
      ]
    ]
  ]


end

to map_player_summaries
  set player_width_outer numPatches / numPlayers
  set player_width_inner player_width_outer * 0.95
  let player_offset player_width_outer * 0.05
  foreach n-values (numPlayers) [?1 -> ?1 + 1 ] [ ?1 ->
    let col_num ?1
    let tempX ((player_width_outer) * (col_num - 1) - 0.501 + player_offset)
    set playerSummaryXLoc lput tempX playerSummaryXLoc
    draw_box ((player_width_outer) * (col_num - 1) - 0.501 + player_offset) (- 0.499) player_width_inner other_player_summary_area (item (col_num - 1) colorList) 1
]

end

to draw_horiz_line [line_x line_y line_l color_num bar_size]

  let overlap_bar bar_size * 1.05

  let remaining_length line_l
  let not_first 0
  while [ remaining_length > 0] [
    if remaining_length < bar_size [set remaining_length bar_size]
    create-borders 1 [set color color_num set size overlap_bar
      setxy line_x + bar_size / 2 + line_l - remaining_length - (overlap_bar - bar_size) * not_first line_y
      set not_first 1
      set heading 90
      stamp die ]
      set remaining_length remaining_length - bar_size
  ]

end

to draw_vert_line [line_x line_y line_l color_num bar_size]

  let overlap_bar bar_size * 1.05

  let remaining_length line_l
  let not_first 0
  while [ remaining_length > 0] [
    if remaining_length < bar_size [set remaining_length bar_size]
    create-borders 1 [set color color_num set size overlap_bar
      setxy line_x line_y + bar_size / 2 + line_l - remaining_length - (overlap_bar - bar_size)  * not_first
      set not_first 1
      set heading 0
      stamp die ]
      set remaining_length remaining_length - bar_size
  ]

end

to draw_box [box_x box_y box_w box_h color_num bar_size]


  let overlap_bar bar_size * 1.05

  let remaining_length box_w
  let not_first 0
  while [ remaining_length > 0] [
    if remaining_length < bar_size [set remaining_length bar_size]
    create-borders 1 [set color color_num set size overlap_bar
      setxy box_x + bar_size / 2 + box_w - remaining_length - (overlap_bar - bar_size) * not_first box_y
      set not_first 1
      set heading 90
      stamp die ]
      set remaining_length remaining_length - bar_size
  ]

  set remaining_length box_w
  set not_first 0
  while [ remaining_length > 0] [
    if remaining_length < bar_size [set remaining_length bar_size]
    create-borders 1 [set color color_num set size overlap_bar
      setxy box_x + bar_size / 2 + box_w - remaining_length - (overlap_bar - bar_size)  * not_first box_y + box_h
      set not_first 1
      set heading 90
      stamp die ]
      set remaining_length remaining_length - bar_size
  ]

  set remaining_length box_h
  set not_first 0
  while [ remaining_length > 0] [
    if remaining_length < bar_size [set remaining_length bar_size]
    create-borders 1 [set color color_num set size overlap_bar
      setxy box_x box_y + bar_size / 2 + box_h - remaining_length - (overlap_bar - bar_size)  * not_first
      set not_first 1
      set heading 0
      stamp die ]
      set remaining_length remaining_length - bar_size
  ]

  set remaining_length box_h
  set not_first 0
  while [ remaining_length > 0] [
    if remaining_length < bar_size [set remaining_length bar_size]
    create-borders 1 [set color color_num set size overlap_bar
      setxy box_x + box_w box_y + bar_size / 2 + box_h - remaining_length - (overlap_bar - bar_size)  * not_first
      set not_first 1
      set heading 0
      stamp die ]
      set remaining_length remaining_length - bar_size
  ]

end

to set-game-parameters

  ;; this procedure takes the list of parameters names and values and processes them for use in the current game

  ;; take the current game's set of parameters
  let currentGameParameters item 0 currentSessionParameters
  set currentSessionParameters sublist currentSessionParameters 1 length currentSessionParameters

  ;; there are two lists - one with variable names, one with values
  (foreach inputFileLabels currentGameParameters [ [?1 ?2] -> ;; first element is variable name, second element is value

    ;; we use a 'parameter handled' structure to avoid having nested foreach statements
    set parameterHandled 0

    ;; if it's the game id, set the game tag as being a practice (if it's 0) or game number otherwise
    if ?1 = "gameID" and parameterHandled = 0[
      ifelse ?2 = 0 [ set gameTag "GP" output-print (word "Game: GP") file-print (word "Game: GP")] [ set gameTag (word "G" ?2) output-print (word "Game: G" ?2) file-print (word "Game: G" ?2)]
      output-print " "
      output-print " "
      output-print "Relevant Game Parameters:"
      output-print " "
      file-print (word ?1 ": " ?2 )
      set parameterHandled 1
    ]

    ;; add any particular cases for parameter handling here [SEE GREEN RESERVES GAME FOR EXAMPLES]



    ;; all other cases not specified above are handled as below - the parameter of the same name is set to the specified value
    if parameterHandled = 0 [  ;; any other case
                               ;;output-print (word ?1 ": " ?2 )
      file-print (word ?1 ": " ?2 )
      let currentParameter []
      ifelse is-string? ?2 [
        set currentParameter (word "set " ?1 "  \"" ?2 "\"" )
      ][
        set currentParameter (word "set " ?1 " " ?2 )
      ]
      set tempLocal currentParameter
      run currentParameter

      set parameterHandled 1
    ]

  ])
  file-print ""

  output-print " "
  output-print " "

end

to read-shock-file
   ;; try to read in the shock parameter data - stop if the file doesn't exist
  if not file-exists? shockListFileName [ ;;if game parameter file is incorrect
    user-message "Please enter valid file name for shock data"
    stop
  ]


  ;; open the file and read it in line by line
  set shockInputs csv:from-file shockListFileName
  set shock_property_labels item 0 shockInputs
  foreach but-first shockInputs [ ?0 ->

    let currentShock ?0

    create-shocks 1 [
    ;; there are two lists - one with variable names, one with values
    (foreach shock_property_labels currentShock [ [?1 ?2] -> ;; first element is variable name, second element is value

      ;; we use a 'parameter handled' structure to avoid having nested foreach statements, in case there are different ways to handle inputs
      set parameterHandled 0

      ;; requirement list and phase shown list may come in as a single value, or may be multiple values
      if parameterHandled = 0 and (?1 = "req" or ?1 = "phase_occur") [  ;; any other case
        file-print (word ?1 ": " ?2 )
        let currentParameter []
        set currentParameter (word "set " ?1 " (list " ?2 ")" )

        run currentParameter
        set parameterHandled 1
      ]

        ;; all other cases not specified above are handled as below - the parameter of the same name is set to the specified value
      if parameterHandled = 0 [  ;; any other case
        ;;output-print (word ?1 ": " ?2 )
        file-print (word ?1 ": " ?2 )
        let currentParameter []
        ifelse is-string? ?2 [
          set currentParameter (word "set " ?1 "  \"" ?2 "\"" )
        ][
          set currentParameter (word "set " ?1 " " ?2 )
        ]
        run currentParameter
        set parameterHandled 1
      ]

    ])

    ]
  ]


  output-print "Shock file read."


end

to read-activity-file



  ;; try to read in the input parameter data - stop if the file doesn't exist
  if not file-exists? activityListFileName [ ;;if game parameter file is incorrect
    user-message "Please enter valid file name for activity data"
    stop
  ]

  ;; open the file and read it in line by line
  set activityInputs csv:from-file activityListFileName
  set activity_property_labels item 0 activityInputs
  foreach but-first activityInputs [ ?0 ->

    let currentActivity ?0

    create-activities 1 [
    ;; there are two lists - one with variable names, one with values
    (foreach activity_property_labels currentActivity [ [?1 ?2] -> ;; first element is variable name, second element is value

      ;; we use a 'parameter handled' structure to avoid having nested foreach statements, in case there are different ways to handle inputs
      set parameterHandled 0

      ;; requirement list and phase shown list may come in as a single value, or may be multiple values
      if parameterHandled = 0 and (?1 = "req" or ?1 = "phase_shown") [  ;; any other case
        file-print (word ?1 ": " ?2 )
        let currentParameter []
        set currentParameter (word "set " ?1 " (list " ?2 ")" )

        run currentParameter
        set parameterHandled 1
      ]

        ;; all other cases not specified above are handled as below - the parameter of the same name is set to the specified value
      if parameterHandled = 0 [  ;; any other case
        ;;output-print (word ?1 ": " ?2 )
        file-print (word ?1 ": " ?2 )
        let currentParameter []
        ifelse is-string? ?2 [
          set currentParameter (word "set " ?1 "  \"" ?2 "\"" )
        ][
          set currentParameter (word "set " ?1 " " ?2 )
        ]
        run currentParameter
        set parameterHandled 1
      ]

    ])

    ]
  ]

  set activityCostList n-values (count activities) [0]
  set activityBenefitList n-values (count activities) [0]
  foreach n-values (count activities) [ ?1 -> ?1 + 1] [ ?1 ->

    ask one-of activities with [ID = ?1] [
      set activityCostList replace-item (?1 - 1) activityCostList cost
      set activityBenefitList replace-item (?1 - 1) activityBenefitList benefit
    ]

  ]

  output-print "Activity file read."


end

to read-end-function-file


  ;; try to read in the input parameter data - stop if the file doesn't exist
  if not file-exists? endFunctionListFileName [ ;;if game parameter file is incorrect
    user-message "Please enter valid file name for end of game function data"
    stop
  ]

  set endFunctionList (list)
  set endFunctionList_String (list)
  set endFunctionActivities (list)
  set endFunctionPhases (list)
  set endFunctionNames (list)
  ;; open the file and read it in line by line
  set endFunctionInputs csv:from-file endFunctionListFileName
  set endFunction_property_labels item 0 endFunctionInputs
  foreach but-first endFunctionInputs [currentFunction ->

    ;; endFunction input file has 6 items in each record:
    ;;0 - the name,
    ;;1 - the activity ids included
    ;;2 - what they are named as in the anonymous command
    ;;3 - the anonymous right hand side
    ;;4 - the anonymous left hand side outcome variable
    ;;5 - whether we are adding to or replacing the LHS variable

    ;;set up the anonymous command, depending on whether the outcome is a member of the activity list, or a different variable
    let temp_lhs []
    let currentString []
    if-else is-string? item 4 currentFunction [
      set temp_lhs item 4 currentFunction
      if-else item 5 currentFunction = "add" [
        set currentString (word "[[playerNumber " item 2 currentFunction "] -> set " temp_lhs " replace-item playerNumber " temp_lhs " ((item playerNumber " temp_lhs ") + (" item 3 currentFunction "))]")
      ][
        set currentString (word "[[playerNumber " item 2 currentFunction "] -> set " temp_lhs " replace-item playerNumber " temp_lhs " (" item 3 currentFunction ")]")
      ]
    ][
      set temp_lhs (word  item 4 currentfunction " tempActivityList")
      if-else item 5 currentFunction = "add" [
      set currentString (word "[[playerNumber " item 2 currentFunction "] -> set tempActivityList item playerNumber playerActivities "
       " set playerActivities replace-item playerNumber playerActivities (replace-item " ((item 4 currentfunction) - 1) " tempActivityList item " ((item 4 currentfunction) - 1) " tempActivityList + (" item 3 currentFunction "))]")
        ;; replace the list of activities for the current player with an updated list that adds the current function value to the identified item for this player
      ][
       set currentString (word "[[playerNumber " item 2 currentFunction "] -> set tempActivityList item playerNumber playerActivities "
       " set playerActivities replace-item playerNumber playerActivities (replace-item " ((item 4 currentfunction) - 1) " tempActivityList (" item 3 currentFunction "))]")
    ;; replace the list of activities for the current player with an updated list that replaces the current function value with the identified item for this player
      ]
    ]

    ;;add function to endFunctionList
    set endFunctionList_String lput currentString endFunctionList_String

    ;;add name to name list
    set endFunctionNames lput item 0 currentFunction endFunctionNames

    ;;add input requirements to list.
    let tempActivities n-values (count activities) [0]
    set tempList (word "set tempList (list " item 1 currentFunction ")")
    run tempList
    foreach tempList [x -> set tempActivities replace-item (x - 1) tempActivities 1]
    set endFunctionActivities lput tempActivities endFunctionActivities


  ]

  output-print "Endgame Function file read."


end

to read-function-file


  ;; try to read in the input parameter data - stop if the file doesn't exist
  if not file-exists? functionListFileName [ ;;if game parameter file is incorrect
    user-message "Please enter valid file name for function data"
    stop
  ]

  set functionList (list)
  set functionList_String (list)
  set functionActivities (list)
  set functionPhases (list)
  set functionNames (list)
  ;; open the file and read it in line by line
  set functionInputs csv:from-file functionListFileName
  set function_property_labels item 0 functionInputs
  foreach but-first functionInputs [currentFunction ->

    ;; function input file has 6 items in each record:
    ;;0 - the name,
    ;;1 - the activity ids included
    ;;2 - what they are named as in the anonymous command
    ;;3 - the anonymous right hand side
    ;;4 - the anonymous left hand side outcome variable
    ;;5 - whether we are adding to or replacing the LHS variable
    ;;6 - the phases it is estimated in

    ;;set up the anonymous command, depending on whether the outcome is a member of the activity list, or a different variable
    let temp_lhs []
    let currentString []
    if-else is-string? item 4 currentFunction [
      set temp_lhs item 4 currentFunction
      if-else item 5 currentFunction = "add" [
        set currentString (word "[[playerNumber " item 2 currentFunction "] -> set " temp_lhs " replace-item playerNumber " temp_lhs " ((item playerNumber " temp_lhs ") + (" item 3 currentFunction "))]")
      ][
        set currentString (word "[[playerNumber " item 2 currentFunction "] -> set " temp_lhs " replace-item playerNumber " temp_lhs " (" item 3 currentFunction ")]")
      ]
    ][
      set temp_lhs (word  item 4 currentfunction " tempActivityList")
      if-else item 5 currentFunction = "add" [
      set currentString (word "[[playerNumber " item 2 currentFunction "] -> set tempActivityList item playerNumber playerActivities "
       " set playerActivities replace-item playerNumber playerActivities (replace-item " ((item 4 currentfunction) - 1) " tempActivityList item " ((item 4 currentfunction) - 1) " tempActivityList + (" item 3 currentFunction "))]")
        ;; replace the list of activities for the current player with an updated list that adds the current function value to the identified item for this player
      ][
       set currentString (word "[[playerNumber " item 2 currentFunction "] -> set tempActivityList item playerNumber playerActivities "
       " set playerActivities replace-item playerNumber playerActivities (replace-item " ((item 4 currentfunction) - 1) " tempActivityList (" item 3 currentFunction "))]")
    ;; replace the list of activities for the current player with an updated list that replaces the current function value with the identified item for this player
      ]
    ]

    ;;add function to functionList
    set functionList_String lput currentString functionList_String

    ;;add name to name list
    set functionNames lput item 0 currentFunction functionNames

    ;;add input requirements to list.
    let tempActivities n-values (count activities) [0]
    set tempList (word "set tempList (list " item 1 currentFunction ")")
    run tempList
    foreach tempList [x -> set tempActivities replace-item (x - 1) tempActivities 1]
    set functionActivities lput tempActivities functionActivities

    ;;add phases applied to list
    let tempPhases n-values (phases + 1) [0] ;;Note - tempPhases is 1-indexed ... the '0' value captures any functions to be executed BEFORE phase 1 starts
    set tempList (word "set tempList (list " item 6 currentFunction ")")
    run tempList
    foreach tempList [x -> set tempPhases replace-item x tempPhases 1] ;; again, note that this is 1-indexed
    set functionPhases lput tempPhases functionPhases

  ]

  output-print "Nonplayer Function file read."


end

to read-nonplayer-function-file


  ;; try to read in the input parameter data - stop if the file doesn't exist
  if not file-exists? nonplayerFunctionListFileName [ ;;if game parameter file is incorrect
    user-message "Please enter valid file name for nonplayer function data"
    stop
  ]

  set nonplayerFunctionList (list)
  set nonplayerFunctionList_String (list)
  set nonplayerFunctionActivities (list)
  set nonplayerFunctionPhases (list)
  set nonplayerFunctionNames (list)
  ;; open the file and read it in line by line
  set nonplayerFunctionInputs csv:from-file nonplayerFunctionListFileName
  set nonplayerFunction_property_labels item 0 nonplayerFunctionInputs
  foreach but-first nonplayerFunctionInputs [currentFunction ->

    ;; function input file has 4 items in each record:
    ;;0 - the name,

    ;;1 - the anonymous right hand side
    ;;2 - the anonymous left hand side outcome variable
    ;;3 - whether we are adding to or replacing the LHS variable
    ;;4 - the phases it is estimated in

    ;;set up the anonymous command, depending on whether the outcome is a member of the activity list, or a different variable
    let temp_lhs []
    let currentString []

    set temp_lhs item 2 currentFunction
    if-else item 3 currentFunction = "add" [
      set currentString (word "set " temp_lhs " " temp_lhs " + (" item 1 currentFunction ")")
    ][
      set currentString (word "set " temp_lhs " (" item 1 currentFunction ")")
    ]


    ;;add function to functionList
    set nonplayerFunctionList_String lput currentString nonplayerFunctionList_String

    ;;add name to name list
    set nonplayerFunctionNames lput item 0 currentFunction nonplayerFunctionNames

    ;;add phases applied to list
    let tempPhases n-values (phases + 1) [0] ;;Note - tempPhases is 1-indexed ... the '0' value captures any functions to be executed BEFORE phase 1 starts
    set tempList (word "set tempList (list " item 4 currentFunction ")")
    run tempList
    foreach tempList [x -> set tempPhases replace-item x tempPhases 1] ;; again, note that this is 1-indexed
    set nonplayerFunctionPhases lput tempPhases nonplayerFunctionPhases

  ]

  output-print "Function file read."


end


to update-endgame-functions


  ;;check each function to see if it should be run in this phase



  foreach n-values ((length endFunctionList_String) ) [?0 -> ?0] [ endFunctionNumber ->



      ;;pull the base string for this function
      let currentFunction_String item endFunctionNumber endFunctionList_String

    show currentFunction_String
      ;;for each player
      foreach n-values (numPlayers) [?1 -> ?1 ] [ playerNumber ->
        let currentActivityList item endFunctionNumber endFunctionActivities
        let currentPlayerActivities item playerNumber playerActivities

        let currentInputs (list)
        foreach n-values (length currentActivityList) [?2 -> ?2] [activityNumber ->
          if item activityNumber currentActivityList > 0 [set currentInputs lput item activityNumber currentPlayerActivities currentInputs ]

        ]



        set currentInputs fput playerNumber currentInputs



        ;;set up a string to run.  i tried to get this to work using anonymous commands, but couldn't figure out how to turn my list of inputs into a set of independent inputs.  this works fine
        let tempA (word currentFunction_String ")")
        foreach n-values (length currentInputs) [?3 -> ?3] [indX ->
          set tempA (word "[" (item (length currentInputs - 1 - indX) currentInputs) "] " tempA)

        ]


        set tempA (word "(foreach " tempA)

        (run tempA)


      ]


  ]
end

to update-functions


  ;;check each function to see if it should be run in this phase


  foreach n-values (length functionList_String) [?0 -> ?0] [ functionNumber ->

    ;;if it should be run, run for each player
    if item (currentPhase) (item functionNumber functionPhases) > 0 [  ;;; be aware that currentPhase is 1-indexed ... if we have a function meant to be done at currentPhase = 0, it is BEFORE start.

      ;;pull the base string for this function
      let currentFunction_String item functionNumber functionList_String

      ;;for each player
      foreach n-values (numPlayers) [?1 -> ?1 ] [ playerNumber ->
        let currentActivityList item functionNumber functionActivities
        let currentPlayerActivities item playerNumber playerActivities

        let currentInputs (list)
        foreach n-values (length currentActivityList) [?2 -> ?2] [activityNumber ->
          if item activityNumber currentActivityList > 0 [set currentInputs lput item activityNumber currentPlayerActivities currentInputs ]

        ]



        set currentInputs fput playerNumber currentInputs

        ;;set up a string to run.  i tried to get this to work using anonymous commands, but couldn't figure out how to turn my list of inputs into a set of independent inputs.  this works fine
        let tempA (word currentFunction_String ")")
        foreach n-values (length currentInputs) [?3 -> ?3] [indX ->
          set tempA (word "[" (item (length currentInputs - 1 - indX) currentInputs) "] " tempA)

        ]


        set tempA (word "(foreach " tempA)

        (run tempA)


      ]

    ]
  ]
end

to update-nonplayer-functions


  ;;check each function to see if it should be run in this phase


  foreach n-values (length nonplayerFunctionList_String) [?0 -> ?0] [ nonplayerFunctionNumber ->

    ;;if it should be run, run for each player
    if item (currentPhase) (item nonplayerFunctionNumber nonplayerFunctionPhases) > 0 [  ;;; be aware that currentPhase is 1-indexed ... if we have a function meant to be done at currentPhase = 0, it is BEFORE start.

      ;;pull the base string for this function
      let currentFunction_String item nonplayerFunctionNumber nonplayerFunctionList_String


      run currentFunction_String


    ]

  ]

end

to update-shocks

  let shock_show_list n-values numPlayers [(list)]
  let loss_show_list n-values numPlayers [n-values count activities [0]]
  let shock_cost_list n-values numPlayers [0]
  let current_points_list n-values numPlayers [0]
  ;;check each shock to see if it should be run in this phase

  foreach n-values numPlayers [?1 -> ?1] [playerNumber ->
   set current_points_list replace-item playerNumber current_points_list (item playerNumber playerResources)
  ]

  ask shocks [

    ;;if it should be run, run for each player
    if member? (currentPhase - 1) phase_occur [ ;; if it's a phase to check it

      if ((ind? = 0 and random-float 1 < pAffect) or ind? = 1) [ ;; don't go any further if it's applicable to the whole group and doesn't pass the dice roll

        ;;if this is not individual, we know already to show the badge for this shock (for all players)
        if (ind? = 0) [
          foreach n-values numPlayers [?1 -> ?1] [playerNumber ->
            let tempShockList item playerNumber shock_show_list
            set shock_show_list replace-item playerNumber shock_show_list (lput shock_name tempShockList)

          ]
        ]

        foreach n-values numPlayers [?1 -> ?1] [playerNumber ->
          if((ind? = 1 and random-float 1 < pAffect) or ind? = 0) [ ;; don't go any further in if it's applied by individual, and this individual doesn't experience the loss

            ;; add the shock to the list for this player, if it's individual specific
            if (ind? = 1) [
              let tempShockList item playerNumber shock_show_list
              set shock_show_list replace-item playerNumber shock_show_list (lput shock_name tempShockList)

            ]
            if-else (is-string? target_act_var) [ ;; we are deducting some value from some variable, with probability

              if random-float 1 < pLose [ ;; if we are experiencing this loss
                let tempWord (word "let currentValue item " playerNumber " " target_act_var " set " target_act_var " replace-item " playerNumber " " target_act_var " max (list 0 (currentValue - amount))")

                run tempWord
              ]

            ][ ;; we are testing the loss of each of some activity, with probability

              let currentLoss 0
              let currentActivityTotal item (target_act_var - 1) (item playerNumber playerActivities)
              let prevTotal currentActivityTotal
              while [currentActivityTotal > 0] [
                if random-float 1 < pLose [set currentLoss currentLoss + 1]
                set currentActivityTotal currentActivityTotal - 1

                let tempLossList (item playerNumber loss_show_list)
                let tempLoss item (target_act_var - 1) tempLossList
                set loss_show_list replace-item playerNumber loss_show_list (replace-item (target_act_var - 1) tempLossList (tempLoss + 1))
              ]
              set playerActivities replace-item playerNumber playerActivities (replace-item (target_act_var - 1) (item playerNumber playerActivities) (prevTotal - currentLoss))
            ]

          ]
        ]


      ]
    ]

  ]

  foreach n-values numPlayers [?1 -> ?1] [playerNumber ->
   set shock_cost_list replace-item playerNumber shock_cost_list ((item playerNumber current_points_list) - (item playerNumber playerResources))
  ]


  ;;now one row for the shock badges and drop in resources, a second row for all of the losses, and a third for any payments the player earned.
  foreach n-values numPlayers [?1 -> ?1] [playerNumber ->
    update-shock-losses playerNumber (item playerNumber shock_show_list) (item playerNumber loss_show_list) (item playerNumber shock_cost_list)
    update-earned-payments playerNumber item playerNumber playerEarnedPayments
  ]

end

to update-displayed-activities [playerNumber]

  ;;kill the numbers that were previously there
  ask integer-agents with [visibleTo = playerNumber and ID = "activityCost"] [die]
  ask integer-agents with [visibleTo = playerNumber and member? "currentSupply" ID] [die]
  ask integer-agents with [visibleTo = playerNumber and member? "currentPool" ID] [die]

  ;;find out which menu we should be looking at
  let currentMenuCount item (playerNumber) [menu] of one-of arrows with [identity = "up"]

  let accessList item playerNumber playerAccess


  ;;make ordered list of all displayable activities
  let available-activities sort-on [ID] activities with [item (ID - 1) accessList > 0]

  ask selections [hubnet-send-override (item (playerNumber) playerNames) self "shape" ["blank"] set visibleTo replace-item playerNumber visibleTo 0 set showing replace-item playerNumber showing 0]
  ask plusses [hubnet-send-override (item (playerNumber) playerNames) self "shape" ["blank"] set visibleTo replace-item playerNumber visibleTo 0 set showing replace-item playerNumber showing 0]
  ask minuses [hubnet-send-override (item (playerNumber) playerNames) self "shape" ["blank"] set visibleTo replace-item playerNumber visibleTo 0 set showing replace-item playerNumber showing 0]

  if not empty? available-activities [
    set available-activities sublist available-activities (currentMenuCount * count selections) (length available-activities)


    ;;show the available activities

    foreach sort-on [counter] selections [ ?1 ->
      ask ?1 [

        let myCounter counter
        if not empty? available-activities [
          let currentID [ID] of first available-activities
          hubnet-send-override (item (playerNumber) playerNames) self "shape" [[image] of first available-activities]
          set visibleTo replace-item playerNumber visibleTo 1
          set showing replace-item playerNumber showing currentID
          let myShowing item playerNumber showing

          ;;show the cost of doing this
          integer-as-agents ([cost] of one-of activities with [ID = myShowing]) 0.5 pink (xcor + size / 3) (ycor + size / 3) playerNumber "activityCost" true

          ;;if this player has access to the activity only through a local supply, show the local supply
          if (item (currentID - 1) (item playerNumber playerAccessBySupply) = 1) [
            integer-as-agents (item (myShowing - 1) (item playerNumber playerConstrainedSupplies)) 0.5 blue (xcor - size / 3) (ycor + size / 3) playerNumber (word "currentSupply " myShowing) true
          ]

          ;;if this activity has a pooled availability (like a loan fund) show the pooled supply
          if not empty? [takes_from] of one-of activities with [ID = myShowing] [
            set tempLocal 0
            run (word "set tempLocal " [takes_from] of one-of activities with [ID = myShowing])
            integer-as-agents tempLocal 0.5 blue (xcor - size / 3) (ycor + size / 3) playerNumber (word "currentPool " myShowing) true
          ]

          ask plusses with [counter = myCounter] [hubnet-send-override (item (playerNumber) playerNames) self "shape" ["badge-plus"]
            set visibleTo replace-item playerNumber visibleTo 1
            show (word "currentActivity " currentID )
            show (word "player " playerNumber)
            update-colors currentID playerNumber
            set showing replace-item playerNumber showing currentID]
          ask minuses with [counter = myCounter] [hubnet-send-override (item (playerNumber) playerNames) self "shape" ["badge-minus"]
            set visibleTo replace-item playerNumber visibleTo 1
            update-colors currentID playerNumber
            set showing replace-item playerNumber showing currentID]

          set available-activities but-first available-activities
        ]
      ]

    ]

  ]

  ;;make sure the arrows appropriately reflect availability of menus
  ask arrows [

    if-else item playerNumber at-end = 0 [

      hubnet-send-override (item playerNumber playerNames) self "color" [arrowNotEndColor]
    ][

        hubnet-send-override (item playerNumber playerNames) self "color" [arrowEndColor]
    ]
  ]

end

to advance-phase


 ;;if there are simulation-level variables that current selections need to add to or negate, update them:
 ;; (note that 'takes_from' are updated in real time to avoid budget overruns, while 'adds_to' are only updated once contributions are confirmed
  ask activities with [not empty? adds_to] [
    foreach n-values numPlayers [?1 -> ?1] [?1 ->
      run (word "set " adds_to " " adds_to " + " item (ID - 1) item ?1 playerCurrentSelections)
    ]
  ]
  ask activities with [is-number? negates] [

    foreach n-values numPlayers [?1 -> ?1] [?1 ->

      let currentDebt item (negates - 1) item ?1 playerActivities
      let currentPayment item (ID - 1) item ?1 playerCurrentSelections
      let newDebt currentDebt - currentPayment
      set playerActivities replace-item ?1 playerActivities (replace-item (negates - 1) (item ?1 playerActivities) newDebt)


    ]
  ]

  ;;if there are payments to be made for supplies used, calculate them

  ;; make a blank slate for earned payments by players across activities
  set playerEarnedPayments n-values numPlayers [n-values count activities [0]]


  ;;for activities with payments
  ask activities with [payment > 0] [
  ;;look for activities that rely on them
    let capitalID ID
    let myPayment payment
    let uses 0
    ask activities with [member? capitalID req] [
  ;; look who is doing those activities without owning the capital

      let useID ID
      foreach n-values numPlayers [?1 -> ?1] [?1 ->
       if (item (capitalID - 1) item ?1 playerActivities = 0) and (item (useID - 1) item ?1 playerCurrentSelections > 0) [
         ;;this player doesn't have the capital but used it for this activity
         set uses uses + (item (useID - 1) item ?1 playerCurrentSelections)
        ]

      ]

    ]
    ;; split among owners
    let totalCapital 0
    foreach n-values numPlayers [?1 -> ?1] [?1 ->
       set totalCapital totalCapital + (item (capitalID - 1) item ?1 playerActivities)
    ]

    if totalCapital > 0 [
      foreach n-values numPlayers [?1 -> ?1] [?1 ->
        if (item (capitalID - 1) item ?1 playerActivities > 0) [ ;;if this is an owner
          let tempPayments item ?1 playerEarnedPayments
          set playerEarnedPayments replace-item ?1 playerEarnedPayments replace-item (capitalID - 1) tempPayments round(uses / totalCapital * (item (capitalID - 1) item ?1 playerActivities) * myPayment)
        ]
      ]
    ]
  ]



  ;;run all appropriate phase functions (yields, interest, etc.)
  update-nonplayer-functions
  update-functions


 ;;update farm counters

  ;;step through all of players' selections and update their points/farms as necessary
  foreach n-values numPlayers [?1 -> ?1] [?1 ->

    let newActivitiesList (map [ [a b] -> a + b] (item ?1 playerActivities) (item ?1 playerCurrentSelections))
    set playerActivities replace-item ?1 playerActivities newActivitiesList
    set playerCurrentSelections replace-item ?1 playerCurrentSelections (n-values (length newActivitiesList) [0])

    set playerResources replace-item ?1 playerResources ((item ?1 playerResources) - item ?1 playerCurrentResources + item ?1 playerPendingBenefits + sum(item ?1 playerEarnedPayments) + endowment)
    set playerCurrentResources replace-item ?1 playerCurrentResources [0]
    set playerPendingBenefits replace-item ?1 playerPendingBenefits [0]



    update-farm-actions (?1)


  ]





  ;;advance the clock
  set currentPhase currentPhase + 1

  ;;if necessary, cycle around to 1 and do what is necessary
  if currentPhase > phases [
    ;;do what needs to be done to start a new yaer
    set currentYear currentYear + 1
    set currentPhase 1

    ;; reset all activties that are 'annual' to zero
    foreach playerPosition [ ?1 ->

      set tempActivityList item (?1 - 1) playerActivities

      ask activities with [annual = 1] [
        set tempActivityList replace-item (ID - 1) tempActivityList 0
      ]
      set playerActivities replace-item (?1 - 1) playerActivities tempActivityList

    ]

  ]

  ;;end game if appropriate
  if currentYear > years [
   ;;do what needs to be done to end the game
    set gameInProgress 0
    ask selections [die]
    ask plusses [die]
    ask minuses [die]
    ask arrows [die]
    ask integer-agents with [ ID = "activityCost"] [die]
    ask integer-agents with [ member? "currentSupply" ID] [die]
    ask integer-agents with [member? "currentPool" ID] [die]
    ask patches with [pxcor >= 0] [set pcolor gray]

    ;; any after-game functions should happen here
    update-endgame-functions
    foreach n-values numPlayers [?1 -> ?1] [?1 ->
      update-farm-actions (?1)
    ]

    stop

  ]


   ;;run all the shocks
  update-shocks

  ;;update round counters
  ask scorecards with [identity = "roundCounter"] [set label (word currentYear " - " currentPhase)]

  ;;set up any cases that need to be handled for particular advances (like different options shown in first phase, e.g.


  ;;reset player variables EXCEPT confirm (this gets reset in the confirm shock procedure)
  foreach playerPosition [ ?1 ->
    ;;set playerConfirm replace-item (?1 - 1) playerConfirm 0
    update-access (?1 - 1)
    update-displayed-activities (?1 - 1)
    update-farm-actions (?1 - 1)
    update-current-selections (?1 - 1)
    ;;ask buttons with [identity = "confirm"] [hubnet-clear-override (item (?1 - 1) playerNames) self "color"]
  ]
  ;update-current-supply
  ;update-takes-from





    ;;if anything that is shown has some minimum requirement, to be done each turn, activate it

  ;;if this activity has a minimum - and we have resources to meet it - then meet that minimum
   foreach playerPosition [?1 ->

    ask selections [


      if item (?1 - 1) visibleTo = 1 [ ;;if we are showing something to this player
        let myShowing item (?1 - 1) showing

        let tempCount [amt_min] of one-of activities with [ID = myShowing]
        while [tempCount > 0] [
          plus-activity (?1 - 1)



          set tempCount (tempCount - 1)
        ]
      ]


    ]

    update-current-selections (?1 - 1)
    update-current-supply
    update-takes-from
  ]


    ;;hide the activities, plusses, and minusses temporarily, and show the between-round shock materials (shock confirm button, plus all changes)
  ask selections [set hidden? true]
  ask plusses [set hidden? true]
  ask minuses [set hidden? true]
  ask arrows [set hidden? true]
  ask integer-agents with [ID = "activityCost"] [set hidden? true]
  ask integer-agents with [member? "currentSupply" ID] [set hidden? true]
  ask integer-agents with [member? "currentPool" ID] [set hidden? true]
  ask buttons with [identity = "confirm_shock"] [set hidden? false]


end

to update-earned-payments [playerNumber player_payments]


  let paymentSum sum player_payments

   ;;show the loss amount
  if paymentSum > 0 [;; if there were any shocks
    integer-as-agents ((paymentSum)) 1 green (numPatches - shock_selection_rel_size * numPatches * 2) (payment_images_y) playerNumber "payment_sum" true

  ]

  ;;now do the loss badges

   ;;;;;;;;;;;;;;;;;;;;;
  ;;count how many unique activities there are for badges
  let currentItemList (list)
  let currentCountList (list)
  foreach n-values (length player_payments) [?1 -> ?1] [ ?1 ->
    if (item ?1 player_payments > 0) and ([badge] of one-of activities with [ID = ?1 + 1] = 1) [
      set currentItemList lput ?1 currentItemList
      set currentCountList lput (item ?1 player_payments) currentCountList
    ]
  ]

  let currentItems length currentItemList

  ;; build badges for player's own farm badge list

  ;;estimate the size and positioning of each one
  let availSpace shock_all_rel_size - shock_selection_rel_size
  let panelSpacing 0
  if currentItems > 1 [
    set panelSpacing availSpace / (currentItems - 1)
  ]

  let startX  numPatches - (numPatches * (shock_all_rel_size + ((1 - shock_all_rel_size) / 2)))

  let tempSize shock_selection_rel_size * numPatches
  ;;add them all in, making them visible only to current player
  foreach n-values currentItems [?1 -> ?1 ] [ ?1 ->
    create-farm-shocks 1 [
      setxy (startX + ?1 * panelSpacing) payment_actions_y

      let tempShape [];
      ask one-of activities with [ID = (item ?1 currentItemList) + 1] [set tempShape image] ;;weird workaround to access the shape name
      set shape tempShape
      set size shock_selection_rel_size * numPatches
      set visibleTo playerNumber
      set ID "shock_loss"
      foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
        if ?2 != playerNumber [
         hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
        ]
      ]
      ;;integer-as-agents (-(item ?1 currentCountList)) 0.3 red (startX + ?1 * panelSpacing + (tempSize * 0.45)) (shock_losses_y - tempSize * 0.45) playerNumber "shock_loss" true
    ]
  ]


end

to update-shock-losses [playerNumber shock_show_list shock_losses shock_costs]

  ;;first the shock badges
  let availSpace (shock_all_rel_size - shock_selection_rel_size) * numPatches

  let panelSpacing 0
  if length shock_show_list > 1 [
    set panelSpacing availSpace / (length shock_show_list - 1)
  ]

  let startX   ( shock_all_rel_size )

  let tempSize shock_selection_rel_size * numPatches
  ;;add them all in, making them visible to all players
  if length shock_show_list > 0 [
    foreach n-values (length shock_show_list) [?1 -> ?1] [ ?1 ->
      create-farm-shocks 1 [
        setxy (startX + ?1 * panelSpacing) shock_images_y


        set shape item ?1 shock_show_list
        set size shock_selection_rel_size * numPatches

        set ID "shock_loss"
        foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
          if ?2 != playerNumber [
            hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
          ]
        ]
      ]
    ]
  ]

   ;;now the loss amount
  if length shock_show_list > 0 [;; if there were any shocks
    integer-as-agents (-(shock_costs)) 1 red (numPatches - shock_selection_rel_size * numPatches * 2) (shock_images_y) playerNumber "shock_loss" true

  ]

  ;;now do the loss badges

   ;;;;;;;;;;;;;;;;;;;;;
  ;;count how many unique activities there are for badges
  let currentItemList (list)
  let currentCountList (list)
  foreach n-values (length shock_losses) [?1 -> ?1] [ ?1 ->
    if (item ?1 shock_losses > 0) and ([badge] of one-of activities with [ID = ?1 + 1] = 1) [
      set currentItemList lput ?1 currentItemList
      set currentCountList lput (item ?1 shock_losses) currentCountList
    ]
  ]

  let currentItems length currentItemList

  ;; build badges for player's own farm badge list

  ;;estimate the size and positioning of each one
  set availSpace shock_all_rel_size - shock_selection_rel_size
  set panelSpacing 0
  if currentItems > 1 [
    set panelSpacing availSpace / (currentItems - 1)
  ]

  set startX  numPatches - (numPatches * (shock_all_rel_size + ((1 - shock_all_rel_size) / 2)))

  set tempSize shock_selection_rel_size * numPatches
  ;;add them all in, making them visible only to current player
  foreach n-values currentItems [?1 -> ?1 ] [ ?1 ->
    create-farm-shocks 1 [
      setxy (startX + ?1 * panelSpacing) shock_losses_y

      let tempShape [];
      ask one-of activities with [ID = (item ?1 currentItemList) + 1] [set tempShape image] ;;weird workaround to access the shape name
      set shape tempShape
      set size shock_selection_rel_size * numPatches
      set visibleTo playerNumber
      set ID "shock_loss"
      foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
        if ?2 != playerNumber [
         hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
        ]
      ]
      integer-as-agents (-(item ?1 currentCountList)) 0.3 red (startX + ?1 * panelSpacing + (tempSize * 0.45)) (shock_losses_y - tempSize * 0.45) playerNumber "shock_loss" true
    ]
  ]


end

to update-farm-actions [playerNumber]
  ;;update the array of 'badge' actions taken in the player farm panel

  ;;kill whatever was there before
  ask farm-actions with [visibleTo = playerNumber] [die]
  ask farm-summaries with [summaryOf = playerNumber] [die]
  ask integer-agents with [visibleTo = playerNumber and ID = "action"] [die]
  ask integer-agents with [summaryOf = playerNumber and ID = "summary"] [die]
  ask integer-agents with [visibleTo = playerNumber and ID = "resource"] [die]
  ask integer-agents with [visibleTo = playerNumber and ID = "selection"] [die]

  ;;;;;;;;;;;;;;;;;;;;;
  ;;count how many unique activities there are for badges
  let currentItemList (list)
  let currentCountList (list)
  let currentActionList (item playerNumber playerActivities)
  foreach n-values (length currentActionList) [?1 -> ?1] [ ?1 ->
    if (item ?1 currentActionList > 0) and ([badge] of one-of activities with [ID = ?1 + 1] = 1) [
      set currentItemList lput ?1 currentItemList
      set currentCountList lput (item ?1 currentActionList) currentCountList
    ]
  ]

  let currentItems length currentItemList

  ;; build badges for player's own farm badge list

  ;;estimate the size and positioning of each one
  let availSpace panel_all_rel_size - panel_selection_rel_size
  let panelSpacing 0
  if currentItems > 1 [
    set panelSpacing availSpace / (currentItems - 1)
  ]

  let startX  (- patches_panel * (panel_all_rel_size + ((1 - panel_all_rel_size) / 2)))

  let tempSize panel_selection_rel_size * patches_panel
  ;;add them all in, making them visible only to current player
  foreach n-values currentItems [?1 -> ?1 ] [ ?1 ->
    create-farm-actions 1 [
      setxy (startX + ?1 * panelSpacing) farm_actions_y
      let tempShape [];
      ask one-of activities with [ID = (item ?1 currentItemList) + 1] [set tempShape image] ;;weird workaround to access the shape name
      set shape tempShape
      set size panel_selection_rel_size * patches_panel
      set visibleTo playerNumber
      foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
        if ?2 != playerNumber [
         hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
        ]
      ]
      integer-as-agents (item ?1 currentCountList) 0.3 white (startX + ?1 * panelSpacing + (tempSize * 0.45)) (farm_actions_y - tempSize * 0.45) playerNumber "action" true
    ]
  ]


   ;;;;;;;;;;;;;;;;
  ;; now build badges for player's summary in others' screens
  ;; other players see BADGES of other players farms, but not the other information

  ;;estimate the size and positioning of each one
  set availSpace player_width_inner / 2
  set panelSpacing 0
  if currentItems > 1 [
    set panelSpacing availSpace / currentItems ;;(currentItems - 1)
  ]

  set startX  item playerNumber playerSummaryXLoc + availSpace / 2

  ;;add them all in, making them invisible only to current player
  foreach n-values currentItems [?1 -> ?1 ] [ ?1 ->
    create-farm-summaries 1 [
      setxy (startX + ?1 * panelSpacing) farm_summary_y
      let tempShape [];
      ask one-of activities with [ID = (item ?1 currentItemList) + 1] [set tempShape image] ;;weird workaround to access the shape name
      set shape tempShape
      set size panel_selection_rel_size * patches_panel
      set summaryOf playerNumber
      hubnet-send-override  (item playerNumber playerNames) self "shape" ["blank"]

      integer-as-agents (item ?1 currentCountList) 0.3 white (startX + ?1 * panelSpacing + (tempSize * 0.45)) (farm_summary_y - tempSize * 0.45) playerNumber "summary" false

    ]
  ]


   ;;;;;;;;;;;;;;;;;;;;;
  ;;count how many unique activities there are for SUMMARIES
  set currentItemList (list)
  set currentCountList (list)
  set currentActionList (item playerNumber playerActivities)
  foreach n-values (length currentActionList) [?1 -> ?1] [ ?1 ->
    if (item ?1 currentActionList > 0) and ([summary] of one-of activities with [ID = ?1 + 1] = 1) [
      set currentItemList lput ?1 currentItemList
      set currentCountList lput (item ?1 currentActionList) currentCountList
    ]
  ]

  set currentItems length currentItemList

  ;; build summaries for player's own farm summary list

  ;;estimate the size and positioning of each one
  set availSpace panel_all_rel_size - panel_selection_rel_size
  set panelSpacing 0
  if currentItems > 1 [
    set panelSpacing availSpace / (currentItems - 1)
  ]

  set startX  (- patches_panel * (panel_all_rel_size + ((1 - panel_all_rel_size) / 2)))

  set tempSize panel_selection_rel_size * patches_panel
  ;;add them all in, making them visible only to current player
  foreach n-values currentItems [?1 -> ?1 ] [ ?1 ->
    create-farm-actions 1 [
      setxy (startX + ?1 * panelSpacing) farm_own_summary_y
      let tempShape [];
      ask one-of activities with [ID = (item ?1 currentItemList) + 1] [set tempShape image] ;;weird workaround to access the shape name
      set shape tempShape
      set size panel_selection_rel_size * patches_panel
      set visibleTo playerNumber
      foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
        if ?2 != playerNumber [
         hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
        ]
      ]
      integer-as-agents (item ?1 currentCountList) 0.5 white (startX + ?1 * panelSpacing + (tempSize * 0.05)) (farm_own_summary_y - tempSize * 0.95) playerNumber "action" true
    ]
  ]

  integer-as-agents (item playerNumber playerResources) 1 green (- patches_panel * 0.6) (numPatches * 0.05) playerNumber "resource" true


end

to update-current-selections [playerNumber]
  ;;update the array of currently selected activities in the player panel

  ;;kill whatever was there before
  ask panel-selections with [visibleTo = playerNumber] [die]
  ask integer-agents with [visibleTo = playerNumber and ID = "selection"] [die]
  ask integer-agents with [visibleTo = playerNumber and ID = "currentResource"] [die]

  ;;count how many unique activities there are
  let currentItemList (list)
  let currentCountList (list)
  let currentCostList (list)
  let currentBenefitList (list)
  let currentSelectionList (item playerNumber playerCurrentSelections)
  foreach n-values (length currentSelectionList) [?1 -> ?1] [ ?1 ->
    if item ?1 currentSelectionList > 0 [
      set currentItemList lput ?1 currentItemList
      set currentCountList lput (item ?1 currentSelectionList) currentCountList
      set currentCostList lput ((item ?1 activityCostList) * (item ?1 currentSelectionList)) currentCostList
      set currentBenefitList lput ((item ?1 activityBenefitList) * (item ?1 currentSelectionList)) currentBenefitList
    ]
  ]

  ;let currentItems length (filter [i -> i > 0] item playerNumber playerCurrentSelections)

  let currentItems length currentItemList

  ;;estimate the size and positioning of each one
  let availSpace panel_all_rel_size - panel_selection_rel_size
  let panelSpacing 0
  if currentItems > 1 [
    set panelSpacing availSpace / (currentItems - 1)
  ]

  let startX  (- patches_panel * (panel_all_rel_size + ((1 - panel_all_rel_size) / 2)))

  let tempSize panel_selection_rel_size * patches_panel
  ;;add them all in, making them visible only to current player
  foreach n-values currentItems [?1 -> ?1 ] [ ?1 ->
    create-panel-selections 1 [
      setxy (startX + ?1 * panelSpacing) panel_selections_y
      let tempShape [];
      ask one-of activities with [ID = (item ?1 currentItemList) + 1] [set tempShape image] ;;weird workaround to access the shape name
      set shape tempShape
      set size tempSize
      set visibleTo playerNumber
      foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
        if ?2 != playerNumber [
         hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
        ]
      ]
    ]
    integer-as-agents (item ?1 currentCountList) 0.3 white (startX + ?1 * panelSpacing + (tempSize * 0.45)) (panel_selections_y - tempSize * 0.45) playerNumber "selection" true

    ]

  set playerCurrentResources replace-item playerNumber playerCurrentResources  (sum currentCostList)
  set playerPendingBenefits replace-item playerNumber playerPendingBenefits (sum currentBenefitList)
  ;;
  integer-as-agents (item playerNumber playerCurrentResources) 1.5 red (- patches_panel * 0.5) (numPatches * 0.5) playerNumber "currentResource" true

end

to arrow-activity [playerNumber currentIdentity currentMenu]

  let accessList item (playerNumber) playerAccess
  let numItems sum accessList
  let sizeMenu count selections


  ;;update the menu number IF we can actually move to a new menu
  if(currentIdentity = "down") and (numItems > sizeMenu * (currentMenu + 1)) [
  ;;request to move down, and there are more items to show
    set currentMenu currentMenu + 1
    ask arrows [set menu replace-item playerNumber menu currentMenu]
  ]

  if(currentIdentity = "up") and (currentMenu > 0) [
  ;;request to move down, and there are more items to show
    set currentMenu currentMenu - 1
    ask arrows [set menu replace-item playerNumber menu currentMenu]
  ]


  ;;make sure all arrows are correctly identified as being at end or not
  ;;make sure the arrow settings are appropriate to the new access menu
  if-else numItems <= sizeMenu * (currentMenu + 1) [
    ;; can't go any further
    ask arrows with [identity = "down"] [set at-end replace-item (playerNumber) at-end 1]
  ][
    ask arrows with [identity = "down"] [set at-end replace-item (playerNumber) at-end 0]
  ]

  if-else currentMenu > 0 [
    ;; can't go any further
    ask arrows with [identity = "up"] [set at-end replace-item (playerNumber) at-end 0]
  ][
    ask arrows with [identity = "up"] [set at-end replace-item (playerNumber) at-end 1]
  ]


  update-displayed-activities playerNumber
  update-current-supply
  update-takes-from
end

to plus-activity [currentMessagePosition]

  let myActivity item currentMessagePosition showing
  let currentMax [amt_max] of one-of activities with [ID = myActivity]



  ;;allow for possibility that max values are specified not as numbers but as other variables
  set tempLocal currentMessagePosition

  if is-string? currentMax [ run (word "set tempLocal " currentMax)

    set currentMax tempLocal]

  let takes_from? false
  set tempLocal 1
  ;;check if this is constrained externally by a 'takes_from' variable
  if( not empty? [takes_from] of one-of activities with [ID = myActivity]) [
    set takes_from? true

    run (word "set tempLocal " [takes_from] of one-of activities with [ID = myActivity])
  ]

  let currentCost [cost] of one-of activities with [ID = myActivity]

  ;; if we can add more of the current activity, do
  let currentCount item (myActivity - 1) (item currentMessagePosition playerCurrentSelections)
  let potentialCost item currentMessagePosition playerCurrentResources + currentCost
  let budget item currentMessagePosition playerResources

  let supplyFlag 1
  if item (myActivity - 1) (item currentMessagePosition playerConstrainedSupplies) = 0 and item (myActivity - 1) (item currentMessagePosition playerAccessBySupply) = 1 [
    ;;this player can only access this thing by supply, and the supply is 0
    set supplyFlag 0
  ]

  if (currentCount < currentMax and potentialCost <= budget and supplyFlag = 1 and tempLocal > 0) [
    set currentCount currentCount + 1
    set playerCurrentSelections (replace-item currentMessagePosition playerCurrentSelections (replace-item (myActivity - 1) (item currentMessagePosition playerCurrentSelections) (currentCount)))

    if takes_from? [
      let currentVar [takes_from] of one-of activities with [ID = myActivity]
      run (word "set " currentVar " " currentVar " - 1")
    ]

  ]

  update-colors myActivity currentMessagePosition


end

to minus-activity [currentMessagePosition]

 let myActivity item currentMessagePosition showing
  let currentMin [amt_min] of one-of activities with [ID = myActivity]
  let currentCost [cost] of one-of activities with [ID = myActivity]

  let takes_from? false
  ;;check if this is constrained externally by a 'takes_from' variable
  if( not empty? [takes_from] of one-of activities with [ID = myActivity]) [
    set takes_from? true
  ]

  ;; if we can subtract more of the current activity, do
  let currentCount item (myActivity - 1) (item currentMessagePosition playerCurrentSelections)
  let potentialCost item currentMessagePosition playerCurrentResources - currentCost
  let budget item currentMessagePosition playerResources
  if (currentCount > currentMin and potentialCost <= budget) [
    set currentCount currentCount - 1
    set playerCurrentSelections (replace-item currentMessagePosition playerCurrentSelections (replace-item (myActivity - 1) (item currentMessagePosition playerCurrentSelections) (currentCount)))

    if takes_from? [
      let currentVar [takes_from] of one-of activities with [ID = myActivity]
      run (word "set " currentVar " " currentVar " + 1")
    ]

  ]

  update-colors myActivity currentMessagePosition

end


to update-takes-from

  ;;for every activity that offers a supply to other activities
  ask activities with [not empty? takes_from] [
    let myID ID
    let myTakes takes_from

    ask selections with [member? myID showing] [ ;; ask the selection showing this activity

      foreach n-values numPlayers [x -> x] [?1 ->   ;; for each of the things they are showing, by player
        if (item ?1 visibleTo) = 1 [
          let currentID item ?1 showing


          ask integer-agents with [visibleTo = ?1 and ID = (word "currentPool " currentID)] [die]
          run (word "integer-as-agents (" myTakes ") 0.5 blue (xcor - size / 3) (ycor + size / 3) " ?1 " (word \"currentPool \" " currentID ") true")

        ]

      ]

    ]

  ]


end

to update-current-supply

  ;;for every activity that offers a supply to other activities
  ask activities with [supply_per > 0] [


  ;;count how many are owned and supplied,
    let assetID ID

    let inUse 0
    set currentSupply 0
    foreach n-values numPlayers [x -> x] [ ?1 ->
       if (item (assetID - 1) (item ?1 playerActivities) = 1) [

        set currentSupply currentSupply + (item (assetID - 1) (item ?1 playerActivities) * supply_per  )
      ]

    ]

    ;;count  how many things requiring it are being done by others who don't own it
    ask activities with [member? assetID req] [


      let reqID ID


      foreach n-values numPlayers [x -> x] [ ?1 ->

        if (item (assetID - 1) (item ?1 playerActivities) = 0) [;; (they don't own it)

          set inUse  (inUse + item (reqID - 1) (item ?1 playerCurrentSelections)) ;; (add the amount of the activity requiring myID that is being done

        ]

      ]

    ]

    ;;now we have the total times asset i is used in other activities j that require it, by others that don't own it
    set currentSupply currentSupply - inUse

  ]


  ask selections  [ ;; ask all the selections

    foreach n-values numPlayers [x -> x] [?1 ->   ;; for each of the things they are showing, by player
      if (item ?1 visibleTo) = 1 [
        let currentID item ?1 showing

        if (item (currentID - 1) (item ?1 playerAccessBySupply) = 1) [   ;;if this player has access to the activity only through a local supply
          let currentReqs []
          ask one-of activities with [ID = currentID] [set currentReqs req]  ;;look across all requirements

          let constrainedSupply min  ([currentSupply] of activities with [member? ID currentReqs]) ;; and find the most constraining supply limit

          ;; save that as the player-specific constraint for this activity
          let currentConstraint item ?1 playerConstrainedSupplies
          set playerConstrainedSupplies replace-item ?1 playerConstrainedSupplies (replace-item (currentID - 1) currentConstraint constrainedSupply)
          ask integer-agents with [visibleTo = ?1 and ID = (word "currentSupply " currentID)] [die]
          integer-as-agents (constrainedSupply) 0.5 blue (xcor - size / 3) (ycor + size / 3) ?1 (word "currentSupply " currentID) true
        ]
      ]

    ]

  ]



end

to update-access [playerNumber]

  ;; get the list of 'how many players are doing each activity right now
  let sumPlayerActivities n-values (count activities) [0]
  let hasSupply sumPlayerActivities
  foreach n-values numPlayers [x -> x][ ?0 ->
    set sumPlayerActivities  (map [ [a b] -> a + b] (item ?0 playerActivities) sumPlayerActivities)
  ]
  ask activities [
   set hasSupply replace-item (ID - 1) hasSupply supply_per
  ]


  let accessList n-values count activities [1]
  let accessBySupply n-values count activities [0]

  ask activities [

    ;; for those that have requirements, check which players meet the reqs - if they miss any
    ;; of them, set to zero UNLESS that requirement has a non-zero 'supply_per' and someone else has it
    foreach req [ ?1 ->

     if item (?1 - 1) (item playerNumber playerActivities) = 0 [ ;; if the player doesn't have this requirement

        if (item (?1 - 1) sumPlayerActivities = 0 or item (?1 - 1) hasSupply = 0) [ ;;i.e., if it isn't a supplied activity that someone else has
          set accessList replace-item (ID - 1) accessList 0
        ]

        if (item (?1 - 1) sumPlayerActivities > 0 and item (?1 - 1) hasSupply > 0) [ ;;i.e., it's a 'supply' var and someone else has it
          set accessBySupply replace-item (ID - 1) accessBySupply 1
        ]

      ]
    ]

    ;; for those that are only visible in certain phases
    if not empty? phase_shown [
      let showNow 0
      ;; set showNow to 1 if we are in any of the phases it should be shown in
      foreach phase_shown [ ?1 ->
        if ?1 = currentPhase [set showNow 1]
      ]

      if showNow = 0 [set accessList replace-item (ID - 1) accessList 0]
    ]
  ]

  set playerAccess replace-item (playerNumber) playerAccess accessList
  set playerAccessBySupply replace-item (playerNumber) playerAccessBySupply accessBySupply


  ;;make sure the arrow settings are appropriate to the new access menu
  if-else sum accessList <= count selections [
    ;; the player only needs one menu, so the arrows should indicate this
    ask arrows [set at-end replace-item (playerNumber) at-end 1]
  ][
    ask arrows with [identity = "up"] [set at-end replace-item (playerNumber) at-end 1]
    ask arrows with [identity = "down"] [set at-end replace-item (playerNumber) at-end 0]
  ]
  ask arrows [set menu replace-item (playerNumber) menu 0]
end

to update-colors [currentCounter playerNumber]

  let currentCount item (currentCounter - 1) (item playerNumber playerCurrentSelections)
  let currentMax [amt_max] of one-of activities with [ID = currentCounter]
  let currentMin [amt_min] of one-of activities with [ID = currentCounter]

  ;if not is-number? amt_max run (word "set amt_max " amt_max
  set tempLocal playerNumber

  if is-string? currentMax [ run (word "set tempLocal " currentMax)

    set currentMax tempLocal]

  ask plusses with [item playerNumber showing = currentCounter] [
    if-else currentCount = currentMax [
      ;; code to gray out and disable plus sign
      hubnet-send-override (item (playerNumber) playerNames) self "color" [gray]
    ] [
      hubnet-send-override (item (playerNumber) playerNames) self "color" [blue]
    ]
  ]

  ask minuses with [item playerNumber showing = currentCounter] [
    if-else currentCount = currentMin [
      ;; code to gray out and disable plus sign
      hubnet-send-override (item (playerNumber) playerNames) self "color" [gray]

    ] [
      hubnet-send-override (item (playerNumber) playerNames) self "color" [blue]

    ]
  ]

end

to process-confirm [playerNumber]

  if item playerNumber playerConfirm = 0 [
    set playerConfirm replace-item playerNumber playerConfirm 1

    ask buttons with [identity = "confirm"] [

      hubnet-send-override (item playerNumber playerNames) self "color" [confirm-down-color]

    ]

  ]
  ;; if EVERYONE has now confirmed, check if we are 'between phases' and otherwise move to advance-phase
  if (sum playerConfirm = numPlayers and [hidden?] of one-of buttons with [identity = "confirm_shock"] = true) [
    advance-phase
  ]

end

to process-confirm-shock [playerNumber]

  if item playerNumber playerConfirmShock = 0 [
    set playerConfirmShock replace-item playerNumber playerConfirmShock 1

    ask buttons with [identity = "confirm_shock"] [

      hubnet-send-override (item playerNumber playerNames) self "color" [confirm-down-color]

    ]

  ]

  ;; if EVERYONE has now confirmed the shock, hide and bring back the activities to move on with this round
  if (sum playerConfirmShock = numPlayers) [
    foreach playerPosition [ ?1 ->
      set playerConfirmShock replace-item (?1 - 1) playerConfirmShock 0
      set playerConfirm replace-item (?1 - 1) playerConfirm 0

      ask buttons with [identity = "confirm"] [hubnet-clear-override (item (?1 - 1) playerNames) self "color"]
      ask buttons with [identity = "confirm_shock"] [hubnet-clear-override (item (?1 - 1) playerNames) self "color"]
    ]

    ask selections [set hidden? false]
    ask plusses [set hidden? false]
    ask minuses [set hidden? false]
    ask arrows [set hidden? false]
    ask integer-agents with [ID = "activityCost"] [set hidden? false]

    foreach playerPosition [ ?1 ->


      ask integer-agents with [member? "currentSupply" ID] [
        if (visibleTo = (?1 - 1)) [
          hubnet-send-override (item (?1 - 1) playerNames) self "hidden?" [false]
        ]
      ]

      ask integer-agents with [member? "currentPool" ID] [

        if (visibleTo = (?1 - 1)) [
          hubnet-send-override (item (?1 - 1) playerNames) self "hidden?" [false]
        ]


      ]
    ]


    update-current-supply
    update-takes-from

    ask buttons with [identity = "confirm_shock"] [set hidden? true]

    ask farm-shocks [die]
    ask integer-agents with [ID = "shock_loss"] [die]
    ask integer-agents with [ID = "payment_sum"] [die]
  ]

end

to integer-as-agents [intI sizeI colI posX posY playerNumber name self?]

  let strI (word intI)
  let offset 0
  while [length strI > 0] [
   let currentI first strI
    ask one-of patches [  ;; a little hack to make this procedure usable both in observer and turtle context - ask a patch to do it.
      sprout-integer-agents 1 [
        setxy  posX + offset posY
        set size sizeI
        if-else currentI = "-" [
          set shape "num-minus"
        ][
          set shape item (read-from-string currentI) number_shape_list]
        set color colI
        set ID name
        if-else self? = true [
          set visibleTo playerNumber
          foreach n-values (numPlayers) [?2 -> ?2 ] [ ?2 ->
            if ?2 != playerNumber [
              hubnet-send-override  (item (?2) playerNames) self "shape" ["blank"]
            ]
          ]

        ] [ ;this number is to be displayed to OTHER players
          set summaryOf playerNumber
          hubnet-send-override  (item playerNumber playerNames) self "shape" ["blank"]

        ]
      ]

    ]
    set strI but-first strI

    set offset offset + sizeI * 0.5
  ]


end
@#$#@#$#@
GRAPHICS-WINDOW
279
10
1367
739
-1
-1
90.0
1
50
1
1
1
0
0
0
1
-4
7
0
7
0
0
0
ticks
30.0

BUTTON
101
323
248
356
Launch Next Game
start-game
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
17
96
158
129
Launch Broadcast
start-hubnet
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
17
135
148
168
Listen to Clients
listen
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

OUTPUT
13
488
253
746
13

INPUTBOX
18
23
247
83
inputParameterFileName
sessionList.csv
1
0
String

INPUTBOX
17
183
246
243
sessionID
101.0
1
0
Number

BUTTON
18
251
151
284
Initialize Session
initialize-session
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This is a basic template for using HubNet in NetLogo to develop multiplayer games.  It includes some of the most basic elements - procedures to run a game, and to listen for / handle HubNet messages.

It also includes a few bits of sample code for key features of a game:

> marking some parts of the view as 'in the game' and other parts as part of the display

> allowing players to manipulate the game surface in a way that is seen by other players, either by changing patch colors or by changing turtle shapes

> having game elements that display differently on each player's screen

This template does not use any NetLogo UI elements in the HubNet client interface.  Instead, we create virtual UI elements in the 'display' portion of the view, for which we have much more visual control (colors, size, etc.)

## HOW IT WORKS

Specific actions in this game template

> Tap on a red square to turn it green, and observe your 'turned green' counter go up

> Tap on a green square to turn it red, and observe your 'turned red' counter go down

> Tap on the 'confirm' button and see the record of it being clicked appear in the host's output monitor


## START-UP INSTRUCTIONS

> 1. Log all of your tablets onto the same network.  If you are in the field using a portable router, this is likely to be the only available wifi network.

> 2. Open the game file on your host tablet.  Zoom out until it fits in your screen

> 3. If necessary, change the language setting on the host.

> 4. Click ‘Launch Broadcast’.  This will reset the software, as well as read in the file containing all game settings.

> 5. Select ‘Mirror 2D view on clients’ on the Hubnet Control Center.

> 6. Click ‘Listen Clients’ on the main screen.  This tells your tablet to listen for the actions of the client computers.  If there ever are any errors generated by Netlogo, this will turn off.  Make sure you turn it back on after clearing the error.

> 7. Open Hubnet on all of the client computers.  Enter the player names in the client computers, in the form ‘PlayerName_HHID’.

> 8. If the game being broadcast shows up in the list, select it.  Otherwise, manually type in the server address (shown in ‘Hubnet Control Center’).

> 9. Click ‘Enter’ on each client.

> 10. Click 'Launch Next Game' to start game.

** A small bug – once you start *EACH* new game, you must have one client exit and re-enter.  For some reason the image files do not load initially, but will load on all client computers once a player has exited and re-entered.  I believe this is something to do with an imperfect match between the world size and the client window size, which auto-corrects on re-entry.  Be sure not to change the player name or number when they re-enter.


## NETLOGO FEATURES

This template exploits the use of the bitmap extension, agent labeling, and hubnet overrides to get around the limitations of NetLogo's visualization capacities.

In the hubnet client, all actual buttons are avoided.  Instead, the world is extended, with patches to the right of the origin capturing elements of the game play, and patches to the left of the origin being used only to display game messages.

Language support is achieved by porting all in-game text to bitmap images that are loaded into the view.

## THINGS TO TRY

This template is meant as a starting point for the development of a dynamic game.  A few of the first things you might try, as exercises to get comfortable with the interface, are:

> Add 'turns'

>> Add a global variable that records whether players have clicked 'Confirm'

>> Adjust the 'Listen' procedure so that once a player clicks 'Confirm', they can't change squares betwen red and green anymore

>> Add a procedure to be run when all players have clicked 'Confirm' - maybe it sends a message to all players, maybe it changes all the colors to something new, maybe it's some cool thing I can't even imagine

>> Within the procedure above, reset the global variable that records 'Confirm' clicks so that players can play again.  Make sure that this procedure is called within the 'Listen' procedure

> Add 'ownership'

>> Add a property to patches that records which player turns them green

>> Adjust the update-patch-state procedure to only change back from green to red, if they were clicked on by the same player who turned them green to begin with

>> Adjust your 'end of turn' procedure to be triggered either by all players clicking confirm, or by all patches in the game being turned green, making each turn a sort of 'resource derby'

>> Adjust the color scheme so that instead of turning red to green, the patches clicked on by players turn from dark gray to the color assigned to that player, so that each player can see who is taking what

> Fill out the panel

>> Create counters that capture the score players accumulate from round to round, and label them (with text, pictures, whatever you think works best)

Now explore what's possible.  Are there spatial interactions that matter?  Is it better to have lots of patches together that are the same color?  Is there something different about a patch sharing a border with another?  Are there other ways that players might interact?

## CREDITS AND REFERENCES

Examples of some of the games published using this approach are:

> Bell, A. R., Rakotonarivo, O. S., Bhargava, A., Duthie, A. B., Zhang, W., Sargent, R., Lewis, A. R., & Kipchumba, A. (2023). Financial incentives often fail to reconcile agricultural productivity and pro-conservation behavior. Communications Earth and Environment, 4(2023), 27. https://doi.org/10.1038/s43247-023-00689-6

> Rakotonarivo, O. S., Bell, A., Dillon, B., Duthie, A. B., Kipchumba, A., Rasolofoson, R. A., Razafimanahaka, J., & Bunnefeld, N. (2021). Experimental Evidence on the Impact of Payments and Property Rights on Forest User Decisions. Frontiers in Conservation Science, 2(July), 1–16. https://doi.org/10.3389/fcosc.2021.661987

> Rakotonarivo, O. S., Jones, I. L., Bell, A., Duthie, A. B., Cusack, J., Minderman, J., Hogan, J., Hodgson, I., & Bunnefeld, N. (2020). Experimental evidence for conservation conflict interventions: The importance of financial payments, community trust and equity attitudes. People and Nature, August, 1–14. https://doi.org/10.1002/pan3.10155

> Rakotonarivo, S. O., Bell, A. R., Abernethy, K., Minderman, J., Bradley Duthie, A., Redpath, S., Keane, A., Travers, H., Bourgeois, S., Moukagni, L. L., Cusack, J. J., Jones, I. L., Pozo, R. A., & Bunnefeld, N. (2021). The role of incentive-based instruments and social equity in conservation conflict interventions. Ecology and Society, 26(2). https://doi.org/10.5751/ES-12306-260208

> Sargent, R., Rakotonarivo, O. S., Rushton, S. P., Cascio, B. J., Grau, A., Bell, A. R., Bunnefeld, N., Dickman, A., & Pfeifer, M. (2022). An experimental game to examine pastoralists’ preferences for human–lion coexistence strategies. People and Nature, June, 1–16. https://doi.org/10.1002/pan3.10393

> Bell, A., & Zhang, W. (2016). Payments discourage coordination in ecosystem services provision: evidence from behavioral experiments in Southeast Asia. Environmental Research Letters, 11, 114024. https://doi.org/10.1088/1748-9326/11/11/114024

> ﻿Bell, A., Zhang, W., & Nou, K. (2016). Pesticide use and cooperative management of natural enemy habitat in a framed field experiment. Agricultural Systems, 143, 1–13. https://doi.org/10.1016/j.agsy.2015.11.012
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

2wt
false
6
Polygon -6459832 true false 120 165 30 105 105 75 165 135 180 150 165 150 105 90 60 105 135 165 135 180
Polygon -2064490 true false 120 150 120 225 210 255 210 195
Polygon -2674135 true false 180 135 255 165 255 225 210 255 210 195 120 150
Line -16777216 false 135 180 165 195
Line -16777216 false 135 195 165 210
Line -16777216 false 134 187 164 202
Circle -7500403 true false 130 205 72
Circle -16777216 true false 136 211 60
Polygon -7500403 true false 184 143 183 103 197 81 209 92 193 104 197 151
Circle -2674135 false false 0 0 298

activity-arrow
true
0
Polygon -7500403 true true 150 0 60 150 105 150 105 293 195 293 195 150 240 150

add-plot
false
12
Circle -6459832 false false 0 0 300
Circle -6459832 true false 72 216 9
Circle -6459832 true false 250 224 9
Circle -6459832 true false 163 274 9
Circle -6459832 true false 151 43 9
Circle -6459832 true false 34 190 9
Circle -6459832 true false 79 246 9
Circle -6459832 true false 35 145 9
Circle -6459832 true false 74 37 9
Circle -6459832 true false 266 73 9
Circle -6459832 true false 207 62 9
Circle -6459832 true false 115 81 9
Circle -6459832 true false 225 125 9
Circle -6459832 true false 162 104 9
Circle -6459832 true false 131 210 9
Circle -6459832 true false 79 169 9
Circle -6459832 true false 105 239 9
Polygon -2674135 true false 105 105 120 105 120 135 150 135 150 165 120 165 120 195 90 195 90 165 60 165 60 135 90 135 90 105
Polygon -2674135 true false 165 255 225 255 225 225 210 225 210 30 180 30 150 60 180 60 180 225 165 225

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

badge-minus
false
9
Rectangle -11221820 true false 45 45 255 255
Polygon -13791810 true true 45 45 45 255 255 255 255 45 45 45 60 240 240 240 240 60 60 60
Polygon -13791810 true true 60 60 60 240 45 255 45 45 60 45
Polygon -13791810 true true 75 120 75 180 120 180 120 180 165 180 180 180 180 180 225 180 225 120 180 120 180 120 120 120 120 120

badge-plus
false
9
Rectangle -11221820 true false 45 45 255 255
Polygon -13791810 true true 45 45 45 255 255 255 255 45 45 45 60 240 240 240 240 60 60 60
Polygon -13791810 true true 60 60 60 240 45 255 45 45 60 45
Polygon -13791810 true true 75 120 75 180 120 180 120 225 165 225 180 225 180 180 225 180 225 120 180 120 180 75 120 75 120 120

bar
true
0
Rectangle -7500403 true true 0 0 300 30

blank
true
0

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

check
false
0
Polygon -7500403 true true 55 138 22 155 53 196 72 232 91 288 111 272 136 258 147 220 167 174 208 113 280 24 257 7 192 78 151 138 106 213 87 182

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

confirm
false
6
Rectangle -10899396 true false 0 60 300 225
Rectangle -13840069 true true 9 72 286 212
Polygon -10899396 true false 75 135 45 165 105 210 270 75 105 165

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

crook
false
0
Polygon -7500403 true true 135 285 135 90 90 60 90 30 135 0 180 15 210 60 195 75 180 30 135 15 105 30 105 60 150 75 150 285 135 285

crop-base
false
0
Polygon -10899396 true false 240 285 285 225 285 165
Polygon -10899396 true false 225 285 210 135 180 75 180 240
Polygon -10899396 true false 225 105 240 105 240 285 225 285
Polygon -10899396 true false 90 270 45 60 45 210
Polygon -10899396 true false 105 270 165 180 165 90 120 180
Polygon -10899396 true false 90 60 90 285 105 285 105 60
Circle -1184463 true false 54 54 42
Circle -1184463 true false 99 69 42
Circle -1184463 true false 54 99 42
Circle -1184463 true false 99 114 42
Circle -1184463 true false 54 144 42
Circle -1184463 true false 99 159 42
Circle -1184463 true false 54 189 42
Circle -1184463 true false 84 24 42
Circle -1184463 true false 234 99 42
Circle -1184463 true false 189 114 42
Circle -1184463 true false 204 69 42
Circle -1184463 true false 234 144 42
Circle -1184463 true false 189 159 42
Circle -1184463 true false 234 189 42
Circle -1184463 true false 189 204 42

cyclone
false
0
Polygon -6459832 true false 60 240 135 120 150 120 93 219 94 238 109 257 78 243 65 255 61 246 40 243
Polygon -10899396 true false 134 124 133 106 185 81 271 84 215 88 277 102 176 100 266 112 170 111 288 122 179 118 257 128 156 122
Line -13791810 false 80 119 148 143
Line -13791810 false 34 78 103 97
Line -13791810 false 74 73 128 94
Line -13791810 false 156 145 252 179
Line -13791810 false 114 167 189 192
Line -13791810 false 162 131 233 147
Line -13791810 false 112 64 156 76
Line -13791810 false 53 165 127 195
Line -13791810 false 164 213 228 244
Line -13791810 false 218 208 265 229
Line -13791810 false 15 172 41 181
Line -13791810 false 9 112 109 154
Line -13791810 false 142 51 256 93
Line -13791810 false 123 226 170 241
Line -13791810 false 206 182 241 187
Line -13791810 false 157 29 227 62
Line -13791810 false 67 215 159 260
Polygon -13791810 true false 109 270 213 266 216 278 184 282 175 277 88 275 75 266 117 261
Circle -13791810 false false -2 -2 304

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

funeral
false
0
Circle -11221820 false false 0 0 300
Circle -11221820 true false 185 136 30
Circle -11221820 true false 105 240 30
Circle -11221820 true false 132 241 28
Circle -11221820 true false 164 226 21
Circle -11221820 true false 165 243 34
Circle -11221820 true false 45 235 30
Circle -11221820 true false 72 232 28
Circle -11221820 true false 191 226 23
Circle -11221820 true false 209 232 30
Rectangle -11221820 true false 57 265 66 282
Rectangle -11221820 true false 84 264 93 282
Rectangle -11221820 true false 113 275 128 293
Rectangle -11221820 true false 146 279 155 298
Rectangle -11221820 true false 177 281 189 295
Rectangle -11221820 true false 214 268 229 285
Rectangle -11221820 true false 200 258 209 282
Polygon -955883 true false 90 120 105 120
Rectangle -11221820 true false 120 135 180 165
Rectangle -11221820 true false 60 150 120 165
Rectangle -11221820 false false 30 165 255 180

grow-2wt
false
12
Circle -10899396 false false 2 1 300
Line -10899396 false 105 240 105 210
Line -10899396 false 135 195 135 165
Line -10899396 false 165 150 165 120
Line -10899396 false 195 105 195 75
Line -10899396 false 225 75 225 45
Line -10899396 false 45 195 45 165
Line -10899396 false 75 135 75 90
Line -10899396 false 120 75 120 45
Line -10899396 false 165 30 165 15
Line -10899396 false 195 255 195 225
Line -10899396 false 225 195 225 150
Line -10899396 false 270 120 270 105
Polygon -10899396 true false 45 182 27 175 27 163 39 172
Polygon -10899396 true false 76 110 95 92 95 78 85 93
Polygon -10899396 true false 104 223 91 223 88 184
Polygon -10899396 true false 134 180 118 170 117 150 128 160
Polygon -10899396 true false 139 193 155 180 151 168 139 188
Polygon -10899396 true false 169 134 181 117 179 101 171 124
Polygon -10899396 true false 199 93 211 72 205 65 199 81
Polygon -10899396 true false 226 63 236 63 243 45 236 45
Polygon -10899396 true false 121 61 105 61 97 28 118 48
Polygon -10899396 true false 200 241 216 232 218 208 198 235
Polygon -10899396 true false 226 171 205 166 205 140 222 160
Polygon -10899396 true false 273 111 252 99 275 84 281 101
Polygon -10899396 true false 166 91 150 91 142 58 163 78
Polygon -10899396 true false 181 216 160 211 160 185 177 205
Polygon -10899396 true false 151 246 130 241 130 215 147 235
Polygon -10899396 true false 260 181 276 172 278 148 258 175
Polygon -10899396 true false 33 111 12 99 35 84 41 101
Polygon -10899396 true false 78 171 57 159 80 144 86 161
Polygon -10899396 true false 109 134 121 117 119 101 111 124
Polygon -10899396 true false 244 224 256 207 254 191 246 214
Polygon -10899396 true false 76 256 60 256 52 223 73 243
Polygon -10899396 true false 168 276 147 264 170 249 176 266
Polygon -10899396 true false 241 126 220 121 220 95 237 115
Polygon -10899396 true false 166 51 145 46 145 20 162 40
Polygon -10899396 true false 91 66 70 61 70 35 87 55
Line -10899396 false 17 142 17 97
Line -10899396 false 83 195 83 150
Line -10899396 false 143 108 143 63
Line -10899396 false 164 90 164 45
Line -10899396 false 129 264 129 219
Line -10899396 false 179 254 179 209
Line -10899396 false 147 291 147 246
Line -10899396 false 75 275 75 230
Line -10899396 false 75 275 75 230
Line -10899396 false 238 163 238 118
Circle -1184463 true false 11 99 14
Circle -1184463 true false 38 158 14
Circle -1184463 true false 68 225 14
Circle -1184463 true false 74 242 14
Circle -1184463 true false 139 240 14
Circle -1184463 true false 186 218 14
Circle -1184463 true false 173 198 14
Circle -1184463 true false 130 172 14
Circle -1184463 true false 126 154 14
Circle -1184463 true false 66 85 14
Circle -1184463 true false 156 116 14
Circle -1184463 true false 221 163 14
Circle -1184463 true false 217 143 14
Circle -1184463 true false 231 111 14
Circle -1184463 true false 263 90 14
Circle -1184463 true false 216 36 14
Circle -1184463 true false 188 66 14
Circle -1184463 true false 133 53 14
Circle -1184463 true false 157 42 14
Circle -1184463 true false 114 36 14
Circle -1184463 true false 251 172 14
Polygon -2674135 true false 57 129 156 142 100 162 24 151
Polygon -2674135 true false 100 160 152 142 148 182 108 195
Polygon -2064490 true false 27 152 99 159 108 193 38 183
Line -16777216 false 62 166 35 163
Line -16777216 false 62 174 39 171
Line -16777216 false 65 181 44 179
Polygon -7500403 true false 68 147 66 137 66 102 83 88 92 105 79 113 79 141
Polygon -7500403 true false 3 147 30 151 34 167 6 167
Polygon -13791810 true false 152 165 180 173 194 157 194 124 213 113 231 90 254 88 254 70 267 70 247 54 243 72 235 81 205 96 199 119 182 124 177 161
Polygon -13791810 true false 138 216 191 263 216 246 244 248 244 232 277 193 277 164 283 147 278 220 235 263 217 283 171 260

grow-high
false
0
Circle -10899396 false false 4 1 300
Line -10899396 false 105 240 105 210
Line -10899396 false 135 195 135 165
Line -10899396 false 165 150 165 120
Line -10899396 false 195 105 195 75
Line -10899396 false 225 75 225 45
Line -10899396 false 45 195 45 165
Line -10899396 false 75 135 75 90
Line -10899396 false 120 75 120 45
Line -10899396 false 165 30 165 15
Line -10899396 false 195 255 195 225
Line -10899396 false 225 195 225 150
Line -10899396 false 270 120 270 105
Polygon -10899396 true false 45 182 27 175 27 163 39 172
Polygon -10899396 true false 76 110 95 92 95 78 85 93
Polygon -10899396 true false 104 223 91 223 88 184
Polygon -10899396 true false 134 180 118 170 117 150 128 160
Polygon -10899396 true false 139 193 155 180 151 168 139 188
Polygon -10899396 true false 169 134 181 117 179 101 171 124
Polygon -10899396 true false 199 93 211 72 205 65 199 81
Polygon -10899396 true false 226 63 236 63 243 45 236 45
Polygon -10899396 true false 121 61 105 61 97 28 118 48
Polygon -10899396 true false 200 241 216 232 218 208 198 235
Polygon -10899396 true false 226 171 205 166 205 140 222 160
Polygon -10899396 true false 273 111 252 99 275 84 281 101
Polygon -10899396 true false 166 91 150 91 142 58 163 78
Polygon -10899396 true false 181 216 160 211 160 185 177 205
Polygon -10899396 true false 151 246 130 241 130 215 147 235
Polygon -10899396 true false 260 181 276 172 278 148 258 175
Polygon -10899396 true false 33 111 12 99 35 84 41 101
Polygon -10899396 true false 78 171 57 159 80 144 86 161
Polygon -10899396 true false 109 134 121 117 119 101 111 124
Polygon -10899396 true false 244 224 256 207 254 191 246 214
Polygon -10899396 true false 76 256 60 256 52 223 73 243
Polygon -10899396 true false 168 276 147 264 170 249 176 266
Polygon -10899396 true false 241 126 220 121 220 95 237 115
Polygon -10899396 true false 166 51 145 46 145 20 162 40
Polygon -10899396 true false 91 66 70 61 70 35 87 55
Line -10899396 false 17 142 17 97
Line -10899396 false 83 195 83 150
Line -10899396 false 143 108 143 63
Line -10899396 false 164 90 164 45
Line -10899396 false 129 264 129 219
Line -10899396 false 179 254 179 209
Line -10899396 false 147 291 147 246
Line -10899396 false 75 275 75 230
Line -10899396 false 75 275 75 230
Line -10899396 false 238 163 238 118
Circle -1184463 true false 11 99 14
Circle -1184463 true false 38 158 14
Circle -1184463 true false 68 225 14
Circle -1184463 true false 74 242 14
Circle -1184463 true false 139 240 14
Circle -1184463 true false 186 218 14
Circle -1184463 true false 173 198 14
Circle -1184463 true false 130 172 14
Circle -1184463 true false 126 154 14
Circle -1184463 true false 66 85 14
Circle -1184463 true false 156 116 14
Circle -1184463 true false 221 163 14
Circle -1184463 true false 217 143 14
Circle -1184463 true false 231 111 14
Circle -1184463 true false 263 90 14
Circle -1184463 true false 216 36 14
Circle -1184463 true false 188 66 14
Circle -1184463 true false 133 53 14
Circle -1184463 true false 157 42 14
Circle -1184463 true false 114 36 14
Circle -1184463 true false 251 172 14

grow-low
false
0
Circle -10899396 false false 0 0 300
Line -10899396 false 105 240 105 210
Line -10899396 false 135 195 135 165
Line -10899396 false 165 150 165 120
Line -10899396 false 195 105 195 75
Line -10899396 false 225 75 225 45
Line -10899396 false 45 195 45 165
Line -10899396 false 75 135 75 90
Line -10899396 false 120 75 120 45
Line -10899396 false 165 30 165 15
Line -10899396 false 195 255 195 225
Line -10899396 false 225 195 225 150
Line -10899396 false 270 120 270 105
Polygon -10899396 true false 45 182 27 175 27 163 39 172
Polygon -10899396 true false 76 110 95 92 95 78 85 93
Polygon -10899396 true false 104 223 91 223 88 184
Polygon -10899396 true false 134 180 118 170 117 150 128 160
Polygon -10899396 true false 139 193 155 180 151 168 139 188
Polygon -10899396 true false 169 134 181 117 179 101 171 124
Polygon -10899396 true false 199 93 211 72 205 65 199 81
Polygon -10899396 true false 226 63 236 63 243 45 236 45
Polygon -10899396 true false 121 61 105 61 97 28 118 48
Polygon -10899396 true false 200 241 216 232 218 208 198 235
Polygon -10899396 true false 226 171 205 166 205 140 222 160
Polygon -10899396 true false 273 111 252 99 275 84 281 101
Circle -1184463 true false 116 50 14
Circle -1184463 true false 129 160 14
Circle -1184463 true false 101 203 14
Circle -1184463 true false 225 165 14
Circle -1184463 true false 220 142 14
Circle -1184463 true false 215 38 14

harvest-2wt
false
0
Circle -10899396 false false 4 1 300
Line -10899396 false 105 240 105 210
Line -10899396 false 135 195 135 165
Line -10899396 false 60 210 60 180
Line -10899396 false 45 225 45 195
Line -10899396 false 60 90 60 60
Line -10899396 false 45 195 45 165
Line -10899396 false 75 135 75 90
Line -10899396 false 120 75 120 45
Line -10899396 false 135 30 135 15
Line -10899396 false 105 210 105 180
Line -10899396 false 60 240 60 195
Line -10899396 false 60 60 60 45
Polygon -10899396 true false 45 182 27 175 27 163 39 172
Polygon -10899396 true false 76 110 95 92 95 78 85 93
Polygon -10899396 true false 104 223 91 223 88 184
Polygon -10899396 true false 134 180 118 170 117 150 128 160
Polygon -10899396 true false 124 223 140 210 136 198 124 218
Polygon -10899396 true false 94 119 106 102 104 86 96 109
Polygon -10899396 true false 94 138 106 117 100 110 94 126
Polygon -10899396 true false 76 153 86 153 93 135 86 135
Polygon -10899396 true false 121 61 105 61 97 28 118 48
Polygon -10899396 true false 80 121 96 112 98 88 78 115
Polygon -10899396 true false 76 216 55 211 55 185 72 205
Polygon -10899396 true false 63 216 42 204 65 189 71 206
Polygon -10899396 true false 106 136 90 136 82 103 103 123
Polygon -10899396 true false 61 156 40 151 40 125 57 145
Polygon -10899396 true false 136 261 115 256 115 230 132 250
Polygon -10899396 true false 35 151 51 142 53 118 33 145
Polygon -10899396 true false 33 111 12 99 35 84 41 101
Polygon -10899396 true false 78 171 57 159 80 144 86 161
Polygon -10899396 true false 109 134 121 117 119 101 111 124
Polygon -10899396 true false 49 134 61 117 59 101 51 124
Polygon -10899396 true false 91 211 75 211 67 178 88 198
Polygon -10899396 true false 108 216 87 204 110 189 116 206
Polygon -10899396 true false 136 141 115 136 115 110 132 130
Polygon -10899396 true false 91 81 70 76 70 50 87 70
Polygon -10899396 true false 91 66 70 61 70 35 87 55
Line -10899396 false 17 142 17 97
Line -10899396 false 83 195 83 150
Line -10899396 false 98 153 98 108
Line -10899396 false 29 210 29 165
Line -10899396 false 129 264 129 219
Line -10899396 false 119 209 119 164
Line -10899396 false 102 246 102 201
Line -10899396 false 75 275 75 230
Line -10899396 false 105 275 105 230
Line -10899396 false 103 103 103 58
Circle -1184463 true false 236 189 14
Circle -1184463 true false 278 158 14
Circle -1184463 true false 263 195 14
Circle -1184463 true false 239 212 14
Circle -1184463 true false 199 225 14
Circle -1184463 true false 216 203 14
Circle -1184463 true false 188 168 14
Circle -1184463 true false 265 127 14
Circle -1184463 true false 201 184 14
Circle -1184463 true false 201 115 14
Circle -1184463 true false 186 116 14
Circle -1184463 true false 221 163 14
Circle -1184463 true false 232 143 14
Circle -1184463 true false 231 111 14
Circle -1184463 true false 248 135 14
Circle -1184463 true false 216 81 14
Circle -1184463 true false 203 96 14
Circle -1184463 true false 253 113 14
Circle -1184463 true false 232 87 14
Circle -1184463 true false 204 141 14
Circle -1184463 true false 251 172 14
Polygon -2674135 true false 120 90 180 105 210 105 150 90
Polygon -2674135 true false 180 105 210 105 210 120 180 120
Polygon -2064490 true false 180 120 120 105 120 90 180 105
Circle -16777216 true false 135 105 30
Polygon -6459832 true false 120 90 90 75 120 60 150 90 135 90 120 75 105 75

harvest-high
false
0
Circle -10899396 false false 4 1 300
Line -10899396 false 105 240 105 210
Line -10899396 false 135 195 135 165
Line -10899396 false 60 210 60 180
Line -10899396 false 45 225 45 195
Line -10899396 false 60 90 60 60
Line -10899396 false 45 195 45 165
Line -10899396 false 75 135 75 90
Line -10899396 false 120 75 120 45
Line -10899396 false 135 30 135 15
Line -10899396 false 105 210 105 180
Line -10899396 false 60 240 60 195
Line -10899396 false 60 60 60 45
Polygon -10899396 true false 45 182 27 175 27 163 39 172
Polygon -10899396 true false 76 110 95 92 95 78 85 93
Polygon -10899396 true false 104 223 91 223 88 184
Polygon -10899396 true false 134 180 118 170 117 150 128 160
Polygon -10899396 true false 124 223 140 210 136 198 124 218
Polygon -10899396 true false 94 119 106 102 104 86 96 109
Polygon -10899396 true false 94 138 106 117 100 110 94 126
Polygon -10899396 true false 76 153 86 153 93 135 86 135
Polygon -10899396 true false 121 61 105 61 97 28 118 48
Polygon -10899396 true false 80 121 96 112 98 88 78 115
Polygon -10899396 true false 76 216 55 211 55 185 72 205
Polygon -10899396 true false 63 216 42 204 65 189 71 206
Polygon -10899396 true false 106 136 90 136 82 103 103 123
Polygon -10899396 true false 61 156 40 151 40 125 57 145
Polygon -10899396 true false 136 261 115 256 115 230 132 250
Polygon -10899396 true false 35 151 51 142 53 118 33 145
Polygon -10899396 true false 33 111 12 99 35 84 41 101
Polygon -10899396 true false 78 171 57 159 80 144 86 161
Polygon -10899396 true false 109 134 121 117 119 101 111 124
Polygon -10899396 true false 49 134 61 117 59 101 51 124
Polygon -10899396 true false 91 211 75 211 67 178 88 198
Polygon -10899396 true false 108 216 87 204 110 189 116 206
Polygon -10899396 true false 136 141 115 136 115 110 132 130
Polygon -10899396 true false 91 81 70 76 70 50 87 70
Polygon -10899396 true false 91 66 70 61 70 35 87 55
Line -10899396 false 17 142 17 97
Line -10899396 false 83 195 83 150
Line -10899396 false 98 153 98 108
Line -10899396 false 29 210 29 165
Line -10899396 false 129 264 129 219
Line -10899396 false 119 209 119 164
Line -10899396 false 102 246 102 201
Line -10899396 false 75 275 75 230
Line -10899396 false 105 275 105 230
Line -10899396 false 103 103 103 58
Circle -1184463 true false 236 189 14
Circle -1184463 true false 278 158 14
Circle -1184463 true false 263 195 14
Circle -1184463 true false 239 212 14
Circle -1184463 true false 199 225 14
Circle -1184463 true false 216 203 14
Circle -1184463 true false 188 168 14
Circle -1184463 true false 265 127 14
Circle -1184463 true false 201 184 14
Circle -1184463 true false 201 115 14
Circle -1184463 true false 186 116 14
Circle -1184463 true false 221 163 14
Circle -1184463 true false 232 143 14
Circle -1184463 true false 231 111 14
Circle -1184463 true false 248 135 14
Circle -1184463 true false 216 81 14
Circle -1184463 true false 203 96 14
Circle -1184463 true false 253 113 14
Circle -1184463 true false 232 87 14
Circle -1184463 true false 204 141 14
Circle -1184463 true false 251 172 14

harvest-low
false
0
Circle -10899396 false false 0 0 300
Line -10899396 false 45 180 45 150
Line -10899396 false 60 150 60 120
Line -10899396 false 90 195 90 165
Line -10899396 false 90 150 90 120
Line -10899396 false 105 195 105 165
Line -10899396 false 30 195 30 165
Line -10899396 false 45 165 45 120
Line -10899396 false 105 120 105 90
Line -10899396 false 90 90 90 75
Line -10899396 false 60 225 60 195
Line -10899396 false 60 195 60 150
Line -10899396 false 120 255 120 240
Polygon -10899396 true false 30 167 12 160 12 148 24 157
Polygon -10899396 true false 46 125 65 107 65 93 55 108
Polygon -10899396 true false 74 178 61 178 58 139
Polygon -10899396 true false 59 165 43 155 42 135 53 145
Polygon -10899396 true false 34 193 50 180 46 168 34 188
Polygon -10899396 true false 79 149 91 132 89 116 81 139
Polygon -10899396 true false 94 108 106 87 100 80 94 96
Polygon -10899396 true false 76 243 86 243 93 225 86 225
Polygon -10899396 true false 76 91 60 91 52 58 73 78
Polygon -10899396 true false 50 166 66 157 68 133 48 160
Polygon -10899396 true false 91 141 70 136 70 110 87 130
Polygon -10899396 true false 108 141 87 129 110 114 116 131
Circle -1184463 true false 191 155 14
Circle -1184463 true false 219 190 14
Circle -1184463 true false 236 143 14
Circle -1184463 true false 225 165 14
Circle -1184463 true false 205 172 14
Circle -1184463 true false 215 143 14

hay
false
0
Polygon -1184463 true false 50 187 97 222 119 234 178 236 219 234 248 211 272 182 205 122 219 50 204 78 203 40 194 95 190 29 178 111 168 22 160 103 142 34 139 98 126 37 128 99 118 45 124 124 96 38 108 114
Polygon -6459832 true false 110 116 138 127 164 126 210 118 214 129 171 144 123 143 102 125 99 121 106 113

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

land-prep-2wt
false
12
Circle -6459832 false false 0 0 300
Circle -6459832 true false 72 216 9
Circle -6459832 true false 250 224 9
Circle -6459832 true false 163 274 9
Circle -6459832 true false 226 268 9
Circle -6459832 true false 79 70 9
Circle -6459832 true false 79 276 9
Circle -6459832 true false 35 220 9
Circle -6459832 true false 74 37 9
Circle -6459832 true false 266 73 9
Circle -6459832 true false 207 62 9
Circle -6459832 true false 115 81 9
Circle -6459832 true false 225 125 9
Circle -6459832 true false 162 104 9
Circle -6459832 true false 146 210 9
Circle -6459832 true false 259 109 9
Circle -6459832 true false 105 239 9
Circle -6459832 true false 34 55 9
Circle -6459832 true false 229 40 9
Circle -6459832 true false 259 130 9
Circle -6459832 true false 259 175 9
Circle -6459832 true false 244 190 9
Circle -6459832 true false 154 235 9
Circle -6459832 true false 79 250 9
Circle -6459832 true false 154 55 9
Circle -6459832 true false 214 100 9
Circle -6459832 true false 124 40 9
Circle -6459832 true false 94 190 9
Circle -6459832 true false 34 145 9
Circle -6459832 true false 169 79 9
Circle -6459832 true false 109 19 9
Circle -6459832 true false 214 19 9
Circle -6459832 true false 184 19 9
Circle -6459832 true false 49 169 9
Circle -6459832 true false 124 169 9
Circle -6459832 true false 154 169 9
Circle -6459832 true false 34 199 9
Circle -6459832 true false 259 199 9
Circle -6459832 true false 94 94 9
Circle -6459832 true false 184 124 9
Circle -6459832 true false 19 109 9
Circle -6459832 true false 79 154 9
Circle -6459832 true false 184 64 9
Circle -6459832 true false 124 244 9
Circle -6459832 true false 19 154 9
Circle -6459832 true false 64 229 9
Circle -6459832 true false 124 274 9
Circle -6459832 true false 214 229 9
Circle -6459832 true false 184 259 9
Polygon -2674135 true false 120 120 225 165 180 180 75 135
Polygon -2674135 true false 180 180 225 165 225 210 180 225
Polygon -2064490 true false 181 224 181 179 76 134 76 179
Circle -7500403 true false 85 169 68
Circle -16777216 true false 89 177 54
Line -16777216 false 90 157 113 167
Line -16777216 false 95 155 118 165
Polygon -6459832 true false 82 134 44 94 81 76 113 119 116 124 121 122 78 66 29 90

land-prep-high
false
12
Circle -6459832 false false 0 0 300
Polygon -6459832 true false 60 105 225 165 210 180 60 120 45 105
Polygon -7500403 true false 195 225 195 195 240 150 240 195 195 240
Polygon -7500403 true false 195 195 180 150 240 150 195 165
Circle -6459832 true false 72 216 9
Circle -6459832 true false 250 224 9
Circle -6459832 true false 163 274 9
Circle -6459832 true false 226 268 9
Circle -6459832 true false 79 70 9
Circle -6459832 true false 79 276 9
Circle -6459832 true false 5 175 9
Circle -6459832 true false 74 37 9
Circle -6459832 true false 266 73 9
Circle -6459832 true false 207 62 9
Circle -6459832 true false 115 81 9
Circle -6459832 true false 225 125 9
Circle -6459832 true false 162 104 9
Circle -6459832 true false 146 210 9
Circle -6459832 true false 259 109 9
Circle -6459832 true false 105 239 9
Circle -6459832 true false 34 55 9
Circle -6459832 true false 244 55 9
Circle -6459832 true false 259 130 9
Circle -6459832 true false 259 175 9
Circle -6459832 true false 169 55 9
Circle -6459832 true false 154 235 9
Circle -6459832 true false 79 250 9
Circle -6459832 true false 154 55 9
Circle -6459832 true false 214 100 9
Circle -6459832 true false 124 40 9
Circle -6459832 true false 94 190 9
Circle -6459832 true false 34 145 9
Circle -6459832 true false 169 79 9
Circle -6459832 true false 109 19 9
Circle -6459832 true false 214 19 9
Circle -6459832 true false 184 19 9
Circle -6459832 true false 49 169 9
Circle -6459832 true false 124 169 9
Circle -6459832 true false 154 169 9
Circle -6459832 true false 34 199 9
Circle -6459832 true false 259 199 9
Circle -6459832 true false 94 94 9
Circle -6459832 true false 184 124 9
Circle -6459832 true false 19 109 9
Circle -6459832 true false 79 154 9
Circle -6459832 true false 49 64 9
Circle -6459832 true false 109 229 9
Circle -6459832 true false 34 229 9
Circle -6459832 true false 64 229 9
Circle -6459832 true false 124 274 9
Circle -6459832 true false 214 229 9
Circle -6459832 true false 184 259 9

land-prep-low
false
12
Circle -6459832 false false 0 0 300
Polygon -6459832 true false 60 105 225 165 210 180 60 120 45 105
Polygon -7500403 true false 195 225 195 195 240 150 240 195 195 240
Polygon -7500403 true false 195 195 180 150 240 150 195 165
Circle -6459832 true false 72 216 9
Circle -6459832 true false 250 224 9
Circle -6459832 true false 163 274 9
Circle -6459832 true false 151 43 9
Circle -6459832 true false 34 190 9
Circle -6459832 true false 79 246 9
Circle -6459832 true false 35 145 9
Circle -6459832 true false 74 37 9
Circle -6459832 true false 266 73 9
Circle -6459832 true false 207 62 9
Circle -6459832 true false 115 81 9
Circle -6459832 true false 225 125 9
Circle -6459832 true false 162 104 9
Circle -6459832 true false 131 210 9
Circle -6459832 true false 79 169 9
Circle -6459832 true false 105 239 9

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

lender-borrow
false
0
Circle -13791810 false false 0 0 300
Polygon -7500403 true true 120 120
Polygon -16777216 true false 31 180 34 179
Polygon -16777216 true false 61 121 50 121 53 85
Rectangle -10899396 true false 119 246 179 276
Polygon -13791810 true false 153 189 144 190 144 214 135 214 149 237 162 214 154 214
Circle -955883 true false 139 251 20
Circle -13791810 true false 94 60 45
Polygon -13791810 true false 132 111 102 111 79 146 99 149 102 182 134 182 136 150 153 146
Rectangle -10899396 true false 168 81 228 111
Circle -955883 true false 188 87 20
Rectangle -10899396 true false 182 98 242 128
Circle -955883 true false 201 105 20
Rectangle -10899396 true false 196 119 256 149
Circle -955883 true false 217 126 20

lender-pay
false
0
Circle -2674135 false false 0 0 300
Polygon -7500403 true true 120 120
Polygon -16777216 true false 31 180 34 179
Polygon -16777216 true false 61 121 50 121 53 85
Rectangle -10899396 true false 119 246 179 276
Polygon -2674135 true false 153 238 144 237 144 213 135 213 149 190 162 213 154 213
Circle -955883 true false 139 251 20
Circle -2674135 true false 94 60 45
Polygon -2674135 true false 132 111 102 111 79 146 99 149 102 182 134 182 136 150 153 146
Rectangle -10899396 true false 168 81 228 111
Circle -955883 true false 188 87 20
Rectangle -10899396 true false 182 98 242 128
Circle -955883 true false 201 105 20
Rectangle -10899396 true false 196 119 256 149
Circle -955883 true false 217 126 20

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

line-thick
true
0
Polygon -7500403 true true 135 0 165 0 165 300 135 300

num-0
false
0
Polygon -7500403 true true 150 30 120 30 90 60 75 105 75 210 90 240 120 270 180 270 210 240 225 210 225 105 210 60 180 30 150 30 150 60 180 60 195 75 210 105 210 210 195 225 180 240 120 240 90 210 90 105 105 75 120 60 150 60

num-1
false
0
Polygon -7500403 true true 120 60 135 30 165 30 165 240 180 255 180 270 120 270 120 255 135 240 135 60

num-2
false
0
Polygon -7500403 true true 75 120 75 90 90 60 105 45 135 30 165 30 195 45 210 60 225 90 225 120 210 150 180 165 150 180 135 195 105 225 105 240 225 240 225 270 75 270 75 210 90 195 120 165 150 150 180 135 195 105 180 75 150 60 135 60 120 75 105 90

num-3
false
0
Polygon -7500403 true true 75 30 225 30 225 60 165 135 195 150 225 180 225 225 210 255 180 270 135 270 90 255 75 225 75 210 105 210 120 240 180 240 195 225 195 195 180 180 135 150 135 120 195 60 75 60

num-4
false
0
Polygon -7500403 true true 75 135 165 30 195 30 195 135 225 135 225 165 195 165 195 270 165 270 165 165 75 165 75 135 165 135 165 60 105 135

num-5
false
0
Polygon -7500403 true true 75 195 105 225 135 240 165 240 195 225 195 180 180 150 150 135 105 150 75 150 75 30 225 30 225 60 105 60 105 120 150 105 195 120 225 150 225 225 195 255 165 270 135 270 90 255 75 240

num-6
false
0
Polygon -7500403 true true 165 30 90 135 75 180 75 210 90 255 120 270 165 270 195 270 210 255 225 210 225 180 210 150 180 135 120 135 105 150 105 180 120 165 165 165 195 180 195 210 195 225 180 240 135 240 105 225 105 195 105 165 105 150 120 135 195 30

num-7
false
0
Polygon -7500403 true true 75 30 225 30 225 60 165 135 120 270 90 270 135 120 180 60 75 60

num-8
false
0
Polygon -7500403 true true 150 15 105 30 90 45 90 75 90 90 90 105 120 120 90 150 75 180 75 210 75 240 90 255 120 270 150 270 180 270 210 255 225 240 225 195 225 180 210 150 180 120 210 105 210 90 210 75 210 45 195 30 150 15 150 45 180 60 180 90 150 105 150 135 180 150 195 180 195 225 180 240 165 240 135 240 120 240 105 225 105 180 120 150 150 135 150 105 120 90 120 75 120 60 150 45 150 45

num-9
false
0
Polygon -7500403 true true 135 270 210 165 225 120 225 90 210 45 180 30 135 30 105 30 90 45 75 90 75 120 90 150 120 165 180 165 195 150 195 120 180 135 135 135 105 120 105 90 105 75 120 60 165 60 195 75 195 105 195 135 195 150 180 165 105 270

num-minus
false
0
Rectangle -7500403 true true 105 135 195 165

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person cutting
false
0
Circle -7500403 true true 65 5 80
Polygon -7500403 true true 60 90 75 195 45 285 60 300 90 300 105 225 120 300 150 300 165 285 135 195 150 90
Rectangle -7500403 true true 82 79 127 94
Polygon -7500403 true true 150 90 195 150 180 180 120 105
Polygon -7500403 true true 60 90 15 150 30 180 90 105
Polygon -6459832 true false 165 210 210 120 240 120 255 120 270 90 285 60 285 30 255 15 210 0 255 45 255 90 225 105 210 105 165 195

person resting
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Line -16777216 false 125 39 140 39
Line -16777216 false 155 39 170 39
Circle -16777216 true false 136 55 15

person walking
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 165 285 135 270 150 225 210 270 255 210 225 225 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 255 105 240 135 165 120
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Polygon -16777216 true false 120 105 150 105 150 90 180 120 150 150 150 135 120 135

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

raindrops
false
0
Polygon -13345367 true false 114 51 97 82 98 101 115 116 138 108 145 84 145 48 144 20
Polygon -13345367 true false 120 156 103 187 104 206 121 221 144 213 151 189 151 153 150 125
Polygon -13345367 true false 190 96 173 127 174 146 191 161 214 153 221 129 221 93 220 65
Polygon -13345367 true false 216 204 199 235 200 254 217 269 240 261 247 237 247 201 246 173
Polygon -13345367 true false 47 179 30 210 31 229 48 244 71 236 78 212 78 176 77 148

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

sheep 2
false
0
Polygon -7500403 true true 209 183 194 198 179 198 164 183 164 174 149 183 89 183 74 168 59 198 44 198 29 185 43 151 28 121 44 91 59 80 89 80 164 95 194 80 254 65 269 80 284 125 269 140 239 125 224 153 209 168
Rectangle -7500403 true true 180 195 195 225
Rectangle -7500403 true true 45 195 60 225
Rectangle -16777216 true false 180 225 195 240
Rectangle -16777216 true false 45 225 60 240
Polygon -7500403 true true 245 60 250 72 240 78 225 63 230 51
Polygon -7500403 true true 25 72 40 80 42 98 22 91
Line -16777216 false 270 137 251 122
Line -16777216 false 266 90 254 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square outline
false
4
Polygon -1184463 true true 15 15 285 15 285 285 15 285 15 30 30 30 30 270 270 270 270 30 15 30

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

vanilla-7
false
0
Polygon -6459832 true false 139 192 140 139 125 122 134 118 148 132 162 115 175 125 158 142 157 193
Circle -10899396 true false 117 104 27
Polygon -13840069 true false 160 191 138 175 138 166 160 156 148 131 153 122 169 153 167 163 147 172 164 184
Polygon -6459832 true false 100 246 94 199 76 185 88 184 96 192 103 165 118 168 106 200 118 245
Circle -10899396 true false 99 151 27
Polygon -13840069 true false 117 243 96 231 96 218 110 206 88 182 73 196 81 247 90 244 79 198 91 193 97 207 92 214 92 237 107 245
Polygon -6459832 true false 203 207 203 165 195 148 204 149 209 156 228 125 232 139 215 169 218 209
Circle -10899396 true false 212 119 31
Circle -10899396 true false 189 140 17
Polygon -13840069 true false 221 206 201 190 201 183 216 177 208 153 214 149 225 184 212 190 224 203
Polygon -6459832 true false 82 137 82 79 69 68 80 61 91 73 118 60 121 71 93 86 93 110 107 104 107 114 95 119 95 140
Circle -10899396 true false 42 37 40
Circle -10899396 true false 108 46 27
Polygon -13840069 true false 110 136 104 104 92 107 80 128 82 104 97 93 96 68 87 72 89 92 72 101 70 136 84 134 100 113
Polygon -6459832 true false 186 136 186 92 195 74 184 74 184 80 169 49 165 61 177 93 177 140
Polygon -13840069 true false 178 133 180 132 191 121 189 110 172 104 172 82 179 62 168 45 166 51 171 64 162 78 164 107 180 118 173 135
Circle -10899396 true false 185 59 22
Polygon -6459832 true false 261 101 261 47 272 38 267 31 258 38 218 25 220 38 252 49 250 102
Polygon -13840069 true false 244 93 268 78 268 66 247 59 261 35 226 23 223 98 230 98 231 28 256 36 242 59 245 72 254 73
Circle -10899396 true false 197 17 24
Polygon -6459832 true false 242 261 242 211 227 190 236 186 248 206 280 178 285 191 254 216 254 231 261 231 262 244 253 244 255 259
Polygon -13840069 true false 240 258 257 244 257 228 235 215 257 195 282 206 271 180 266 189 266 191 254 190 231 216 245 233 245 240 237 253
Circle -10899396 true false 214 169 31

vsla-borrow
false
0
Circle -13791810 false false 0 0 300
Polygon -7500403 true true 120 120
Polygon -16777216 true false 31 180 34 179
Polygon -13791810 true false 17 180 46 59 60 59 89 181 73 180 64 136 45 136 34 178
Polygon -13791810 true false 73 62 102 179 118 179 147 61 133 61 112 150 89 61
Polygon -13791810 true false 163 63 131 181 190 181 195 165 149 166 164 122 195 123 202 108 169 108 176 76 207 76 212 61
Polygon -13791810 true false 199 179 223 59 264 59 263 76 231 75 214 165 258 164 257 179
Polygon -16777216 true false 61 121 50 121 53 85
Rectangle -10899396 true false 119 246 179 276
Polygon -13791810 true false 153 189 144 190 144 214 135 214 149 237 162 214 154 214
Circle -955883 true false 139 251 20

vsla-join
false
0
Circle -10899396 false false 0 0 300
Polygon -7500403 true true 120 120
Polygon -16777216 true false 31 180 34 179
Polygon -10899396 true false 17 180 46 59 60 59 89 181 73 180 64 136 45 136 34 178
Polygon -10899396 true false 73 62 102 179 118 179 147 61 133 61 112 150 89 61
Polygon -10899396 true false 163 63 131 181 190 181 195 165 149 166 164 122 195 123 202 108 169 108 176 76 207 76 212 61
Polygon -10899396 true false 199 179 223 59 264 59 263 76 231 75 214 165 258 164 257 179
Polygon -16777216 true false 61 121 50 121 53 85
Polygon -10899396 true false 165 287 133 287 133 238 106 253 149 190 192 253 165 242

vsla-pay
false
0
Circle -2674135 false false 0 0 300
Polygon -7500403 true true 120 120
Polygon -16777216 true false 31 180 34 179
Polygon -2674135 true false 17 180 46 59 60 59 89 181 73 180 64 136 45 136 34 178
Polygon -2674135 true false 73 62 102 179 118 179 147 61 133 61 112 150 89 61
Polygon -2674135 true false 163 63 131 181 190 181 195 165 149 166 164 122 195 123 202 108 169 108 176 76 207 76 212 61
Polygon -2674135 true false 199 179 223 59 264 59 263 76 231 75 214 165 258 164 257 179
Polygon -16777216 true false 61 121 50 121 53 85
Rectangle -10899396 true false 119 246 179 276
Polygon -2674135 true false 153 236 144 235 144 211 135 211 149 188 162 211 154 211
Circle -955883 true false 139 251 20

vsla-share
false
0
Circle -10899396 false false 0 0 300
Polygon -7500403 true true 120 120
Polygon -16777216 true false 31 180 34 179
Polygon -10899396 true false 17 180 46 59 60 59 89 181 73 180 64 136 45 136 34 178
Polygon -10899396 true false 73 62 102 179 118 179 147 61 133 61 112 150 89 61
Polygon -10899396 true false 163 63 131 181 190 181 195 165 149 166 164 122 195 123 202 108 169 108 176 76 207 76 212 61
Polygon -10899396 true false 199 179 223 59 264 59 263 76 231 75 214 165 258 164 257 179
Polygon -16777216 true false 61 121 50 121 53 85
Rectangle -10899396 true false 119 246 179 276
Polygon -10899396 true false 154 237 145 236 145 212 136 212 150 189 163 212 155 212
Circle -955883 true false 139 251 20

wedding
false
0
Circle -2064490 true false 105 61 30
Circle -2064490 true false 155 61 30
Rectangle -2064490 true false 98 99 143 144
Rectangle -2064490 true false 111 139 129 203
Rectangle -2064490 true false 155 100 182 199
Polygon -2064490 true false 60 210 60 90 75 60 90 45 150 15 180 30 210 45 225 60 240 90 240 210 225 210 225 90 210 60 150 30 90 60 75 90 75 210
Circle -2064490 true false 105 240 30
Circle -2064490 true false 132 241 28
Circle -2064490 true false 164 226 21
Circle -2064490 true false 165 243 34
Circle -2064490 true false 45 235 30
Circle -2064490 true false 72 232 28
Circle -2064490 true false 191 226 23
Circle -2064490 true false 209 232 30
Rectangle -2064490 true false 57 265 66 282
Rectangle -2064490 true false 84 264 93 282
Rectangle -2064490 true false 113 275 128 293
Rectangle -2064490 true false 146 279 155 298
Rectangle -2064490 true false 177 281 189 295
Rectangle -2064490 true false 214 268 229 285
Rectangle -2064490 true false 200 258 209 282
Circle -2064490 false false 0 0 300

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
VIEW
7
10
1087
730
0
0
0
1
1
1
1
1
0
1
1
1
-4
7
0
7

@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
