/*global
 jquery:true,
 $:true,
 _: true,
 us: true,
 gameState: true,
 window: true */

var us = _.noConflict();

var view = (function (us) {
  "use strict";
  var self = {}

  /*
   * DEPRECATED
  */
  var render = function(state) {
    console.log("Render is Deprecated, Do Not Use. Use view.update(gameState) instead")

    var getViewsForLane = function(laneData) {
      var getViewForCard = function(cardData) {
        return $('<div />', {
          "class": "card",
          html: ("<div class='cardLabelTop'>" + cardData.cardType + "</div>" +
                 "<div class='cardLabelBottom'>" + cardData.cardType + "</div>")
        }).addClass(cardData.color)
      }

      var getViewForBaseCard = function (modifierCards) {
        var modifiers = us.reduce(modifierCards, function (memo, card) {
          return memo + card.cardType.charAt(0) + card.cardType.charAt(1) + ","
        }, "")

        if (modifiers) { modifiers = '<br>(' + modifiers + ')' }

        return $('<div />', {
          "class": "base card none",
          html: ("base" + modifiers)
        })
      }

      var laneView = $("<div />");
      var i = 0

      // The base itself
      laneView.append(getViewForBaseCard(laneData.modifiers).attr({
        toLocation: 'base',
        fromLocation: "hand",
        baseId: laneData.id
      }))
      // Top stack
      for (i = 0; i < laneData.p1.length; i++) {
        // css fix for cards so they stack going upwards instead of down
        laneView.append(getViewForCard(laneData.p1[i]).attr({
          location: 'base_p1',
          baseId: laneData.id
        }).css('margin-top', -180-i*30))
      }
      // Bottom stack
      for (i = 0; i < laneData.p2.length; i++) {
        laneView.append(getViewForCard(laneData.p2[i]).attr({
          location: 'base_p2',
          baseId: laneData.id
        }).css('margin-top', 60+i*30))
      }

      return laneView
    }

    var renderPlayerHandAndDiscard = function (playerName, index) {
      var playerData = state[playerName]

      // Render the cards in their hand and discard
      us.each(["hand", "discard"], function (position) {
        // Clear old card color
        $("#player" + (index + 1) + "-" + position).removeClass('red blue green none')
        if (playerData[position.toLowerCase()]) {
          // Set new color
          $("#player" + (index + 1) + "-" + position).addClass(playerData[position.toLowerCase()].color)
          // Set card type label
          $("#player" + (index + 1) + "-" + position).text(playerData[position.toLowerCase()].cardType)
        } else {
          // Clear card type label
          $("#player" + (index + 1) + "-" + position).text(position)
        }
      })
    }

    us.each(["p1", "p2"], function (playerName, index) {
      renderPlayerHandAndDiscard(playerName, index)
    })

    $('.lane').html("");
    us.each(state.bases, function (laneData, currentLaneNumber) {
      $('.lane').eq(currentLaneNumber).html(getViewsForLane(laneData))
    })
  }
  self.render = render

  /*
   * Public - Updates the view to gameState passed in, attempting to alter
   * only what has changed since the last update
  */
  var update = function (state) {
    
    // = Update Hands and Discards =
    
    var playerData
    
    us.each(["player-1", "player-2"], function (playerName, playerNumber) {
      playerData = state["p" + (playerNumber + 1)] // TODO: update naming to fix convention
      
      us.each(["hand", "discard"], function (position) {
        var positionUIElement = $("#" + playerName + "-" + position)
        // Clear old card color
        positionUIElement.removeClass('red blue green none')
        // If there's a card in that position, show it, otherwise show the placeholder
        if (playerData[position]) {
          // Set new color
          positionUIElement.addClass(playerData[position.toLowerCase()].color)
          // Set card type label
          positionUIElement.text(playerData[position.toLowerCase()].cardType)
        } else {
          // Clear card type label
          positionUIElement.text(position)
        }
      })
    })
    
    // = Update Lanes =
    
    // If a lane was added or removed since last update, 
    // then all lanes have most likely shifted position and must be re-rendered
    if ($('.base').length != state.bases.length) {
      $('.lane').children(".card").hide().not(".base").removeClass("red blue green none")
    }
    
    $('.lane').each(function (currentLaneNumber) { 
      var currentLaneUI = $(this)
      if (currentLaneNumber < state.bases.length) {
        // update the base
        currentLaneUI.find(".base")
        .html("<br /><b>Base</b>" + 
          us.reduce(state.bases[currentLaneNumber].modifiers, function (memo, card) {
            return memo + "<i>[" + card.cardType + "]</i><br />"
          }, "<br />"))
        .attr('baseid', state.bases[currentLaneNumber].id)
        .show()
        
        // update the stacks
        us.each(['p1', 'p2'], function (stackDirection) {
          // Get the first empty card spot and start updating from there.
          // Since the only thing a player can do is add cards to a stack,
          // there's no need to bother with anything that was already rendered
          var stackUI = currentLaneUI.children("." + stackDirection + "stack")
          var cardNumber = stackUI.filter(":visible").length
          var stack = state.bases[currentLaneNumber][stackDirection]
          var cardsInStack = stack.length
          var cardData
          
          while (cardNumber < cardsInStack) {
            cardData = stack[cardNumber]
            stackUI.eq(cardNumber)
                   .text(cardData.cardType)
                   .addClass(cardData.color)
                   .show()
            
            cardNumber++
          }            
        })
      } else {
        $(this).find(".card").hide().not(".base").removeClass("red blue green none")
      }
    })
  }
  self.update = update

  /*
   * Public - shows a notification
  */
  var showNotification = function (notificationType) {
    $('#connecting-notification').slideUp(200)
    if (notificationType === 'connecting') {
      // Show the connecting message
      $('#connecting-notification').slideDown(200)
    }
    else {
      $('#connecting-notification').text(notificationType).slideDown(200)
    }
  }
  self.showNotification = showNotification

  /*
   * Public - clears notification
  */
  var clearNotification = function () {
    $('#connecting-notification').slideUp(200)
  }
  self.clearNotification = clearNotification

  /*
   * Public - Displays the current game id to all connected players
  */
  var displayGameID = function (gameID) {
    $('#game-id').text("Game ID: " + gameID + " ")
  }
  self.displayGameID = displayGameID

  /*
   * Public - Prompts the player to start a new game or choose an existing one
  */
  var showChooseGameDialog = function () {
    $('#choose-game-dialog').fadeIn(200)
  }
  self.showChooseGameDialog = showChooseGameDialog

  /*
   * Public - Promps the player to choose player1, player2, or spectator
  */
  var showChoosePlayerTypeDialog = function (playerSlots) {
    $('#choose-player-type-dialog').fadeIn(200)
    
    us.each(["player1", "player2"], function (player) {
      if (playerSlots[player]) {
        $('#choose-player-type-dialog #' + player).removeClass('disabled')
      } else {
        $('#choose-player-type-dialog #' + player).addClass('disabled')
      }
    })
  }
  self.showChoosePlayerTypeDialog = showChoosePlayerTypeDialog

  /*
   * Public - Promps the player to choose a character (deck) to use
  */
  var showChooseCharacterDialog = function () {
    $('#choose-character-dialog').fadeIn(200)
  }
  self.showChooseCharacterDialog = showChooseCharacterDialog

  /*
   * Public - Shows the play area to the user
  */
  var showPlayArea = function () {
    $('#play-area').show(500)
  }
  self.showPlayArea = showPlayArea

  /*
   * Public - Sets the labels for the decks for both players
  */
  var setPlayerDeckNames = function (p1DeckName, p2DeckName) {
    $('#player-1-deck').text(p1DeckName)
    $('#player-2-deck').text(p2DeckName)
  }
  self.setPlayerDeckNames = setPlayerDeckNames

  /*
   * Public - Initializes the view
  */
  var init = function (server) {
    // Play on top/bottom of lane when the lane or the lane itself is clicked
    $('.lane').click(function (event) {
      // Ignore clicked base cards and allow the click to keep bubbling up
      if (!$(event.target).hasClass("base")) {
        server.sendCardMove("hand",
          ((event.pageY - $(this).offset().top < $(this).height() / 2) ? "base_p1" : "base_p2"),
          ($(this).find('div.base').first().attr('baseid')))
        event.stopPropagation()
      }
    })
    $('#play-area').click(function (event) {
      // Catch bubbled up card events
      if ($(event.target).hasClass('card')) {
        server.sendCardMove($(event.target).attr("fromLocation"),
                     $(event.target).attr("toLocation"),
                     $(event.target).attr("baseid"))
      }
      // Else attempt to play a new base on the left or right
      else {
        var to = (event.pageX - $(this).offset().left < $(this).width() / 2) ? "base_left" : "base_right"
        server.sendCardMove("hand", to)
      }
    })

    // Choose player type dialog
    $('#choose-player-type-dialog .player-type-choice').click(function (event) {
      if (!$(event.target).hasClass("disabled")) {
        $('#choose-player-type-dialog').fadeOut(200)

        if ($(event.target).attr("choice") !== "spectator") {
          server.sendJoin($(event.target).attr("choice"))
        } else {
          server.sendRequestGameState()
          showPlayArea()
        }        
      }
    })

    // Choose character dialog
    $('#choose-character-dialog .choice').click(function (event) {
      $('#choose-character-dialog').fadeOut(200)
      server.sendChooseCharacterMessage($(event.target).attr("id"))
    })

    // Choose new game or join existing game dialog
    $('#new-game').click(function (event) {
      $('#choose-game-dialog').fadeOut(200)
      $.get('http://' + window.location.host + '/brawl/new_game', function (data, textStatus, xhr) {
        var gameID = JSON.parse(data).gameID
        
        // Show the gameID to the user so they can send it to friends
        displayGameID(gameID)

        // TODO: fix key so it's gameID
        server.sendConnect(gameID)
      })
    })
    $('#existing-game').click(function (event) {
      var gameID = $('#existing-game-container input').val()
      $('#choose-game-dialog').fadeOut(200)

      // Show the gameID to the user so they can send it to friends
      displayGameID(gameID)

      server.sendConnect(gameID)
    })
    $('#existing-game-container input').focus(function(event) {
      $(this).val("")
    })

    /* Keyboard shortcuts */
    $(window.document).keydown(function (event) {
      if (event.which === 65 || event.which === 186) {
        $('#player-1-deck').click()
      }
      else if (event.which === 81 || event.which === 80) {
        $('#player-1-hand').click()
      }
      // TODO: remove
      else if (event.which == 68) {
        update()
      }
    })
  }
  self.init = init

  return self
}(us))
