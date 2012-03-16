/*global
 jquery:true,
 $:true,
 _: true,
 us: true,
 gameState: true,
 window: true,
 setTimeout */

var us = _.noConflict();

var view = (function (us) {
  "use strict";
  var self = {}

  // Private Vars
  var animationDuration = 200 // in ms

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
    if ($('.base').filter(":visible").length !== state.bases.length) {
      $('.lane').children(".card").hide().not(".base").removeClass("red blue green none")
    }

    $('.lane').each(function (currentLaneNumber) {
      var currentLaneUI = $(this)
      if (currentLaneNumber < state.bases.length) {
        // update the base
        currentLaneUI.find(".base")
        .html("<br />Base" +
          us.reduce(state.bases[currentLaneNumber].modifiers, function (memo, card) {
            return memo + "[" + card.cardType + "]<br />"
          }, "<br />"))
        .attr('baseid', state.bases[currentLaneNumber].id)
        .show()

        // update the stacks
        us.each(['p1', 'p2'], function (stackDirection) {
          // Get the first empty card spot and start updating from there.
          // Since the only thing a player can do is add cards to a stack,
          // there's no need to bother with anything that was already rendered
          var stackUI = currentLaneUI.children("." + stackDirection + "stack"),
              cardNumber = stackUI.filter(":visible").length,
              stack = state.bases[currentLaneNumber][stackDirection],
              cardsInStack = stack.length,
              cardData

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
    $('#connecting-notification').slideUp(animationDuration)
    if (notificationType === 'connecting') {
      // Show the connecting message
      $('#connecting-notification').slideDown(animationDuration)
    }
    else {
      $('#connecting-notification').text(notificationType).slideDown(animationDuration)
    }
  }
  self.showNotification = showNotification

  /*
   * Public - clears notification
  */
  var clearNotification = function () {
    $('#connecting-notification').slideUp(animationDuration).text("")
  }
  self.clearNotification = clearNotification

  /*
   * Public - Fetches the message currently displayed on the notification
  */
  var getNotificationMessage = function () {
    return $('#connecting-notification').text()
  }
  self.getNotificationMessage = getNotificationMessage

  /*
   * Public - Displays the current game id to all connected players
  */
  var displayGameID = function (gameID) {
    $('#game-id').fadeTo(animationDuration, 1.0)
    $('#game-id').text("Game ID: " + gameID + " ")
    setTimeout(function () {
      $('#game-id').fadeTo(animationDuration, 0.5) // fade to 30% opacity
    }, 5000)
  }
  self.displayGameID = displayGameID

  /*
   * Public - Informs the user that the server couldn't be reached and the game won't run
  */
  var showFailedToConnectMessage = function () {
    $('#failed-to-connect').show()
  }
  self.showFailedToConnectMessage = showFailedToConnectMessage

  /*
   * Public - Informs the user that their browser does not support websockets and they should
   * consider getting a different one for the time being
  */
  var showWebsocketsNotSupportedMessage = function () {
    $('#websockets-not-supported').show()
  }
  self.showWebsocketsNotSupportedMessage = showWebsocketsNotSupportedMessage

  /*
   * Public - Prompts the player to start a new game or choose an existing one
  */
  var showChooseGameDialog = function () {
    $('#choose-game-dialog').fadeIn(animationDuration)
  }
  self.showChooseGameDialog = showChooseGameDialog

  /*
   * Public - Promps the player to choose player1, player2, or spectator
  */
  var showChoosePlayerTypeDialog = function (playerSlots) {
    $('#choose-player-type-dialog').fadeIn(animationDuration)

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
    $('#choose-character-dialog').fadeIn(animationDuration)
  }
  self.showChooseCharacterDialog = showChooseCharacterDialog

  /*
   * Public - Informs the player of the winner of the match and allows a rematch request
  */
  var showGameOverDialog = function (message) {
    $('#game-over-dialog').fadeIn(animationDuration)
    $('#game-over-dialog #winner').text(message)
  }
  self.showGameOverDialog = showGameOverDialog


  /*
   * Public - Shows the play area to the user
  */
  var showPlayArea = function () {
    $('.dialog, #logo').fadeOut(animationDuration)

    // Reset all cards to hidden so the first render is from scratch
    $('.lane').children(".card").hide().not(".base").removeClass("red blue green none")
    
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
    // Card clicked events for all base cards and players' hands/decks/discards
    $('.base.card, .player-area .card').click(function (event) {
      server.sendCardMove($(event.target).attr("fromLocation"),
                          $(event.target).attr("toLocation"),
                          $(event.target).attr("baseid"))
      event.stopPropagation()
    })
    // Play on top/bottom of lane when the lane or the lane itself is clicked
    $('.lane').click(function (event) {
      server.sendCardMove("hand",
        ((event.pageY - $(this).offset().top < $(this).height() / 2) ? "base_p1" : "base_p2"),
        ($(this).find('div.base').first().attr('baseid')))
      event.stopPropagation()
    })
    $('#play-area').click(function (event) {
      var to = (event.pageX - $(this).offset().left < $(this).width() / 2) ? "base_left" : "base_right"
      server.sendCardMove("hand", to)
      event.stopPropagation()
    })

    // Choose new game or join existing game dialog
    $('#new-game').click(function (event) {
      $('#choose-game-dialog').fadeOut(animationDuration)
      $.get('http://' + window.location.host + '/brawl/new_game', function (data, textStatus, xhr) {
        var gameID = JSON.parse(data).gameID

        // Show the gameID to the user so they can send it to friends
        displayGameID(gameID)

        server.sendConnect(gameID)
      })
    })
    $('#join-game').click(function (event) {
      var gameID = $('#existing-game input').val()
      $('#choose-game-dialog').fadeOut(animationDuration)

      // Show the gameID to the user so they can send it to friends
      displayGameID(gameID)

      server.sendConnect(gameID)
    })
    $('#existing-game input').focus(function (event) {
      $(this).val("")
    })

    // Choose player type dialog
    $('#choose-player-type-dialog .player-type-choice').click(function (event) {
      if (!$(event.target).hasClass("disabled")) {
        $('#choose-player-type-dialog').fadeOut(animationDuration)

        if ($(event.target).attr("choice") !== "spectator") {
          server.sendJoin($(event.target).attr("choice"))
        } else {
          server.sendGetGameState()
          showPlayArea()
        }
      }
    })

    // Choose character dialog
    $('#choose-character-dialog .choice').click(function (event) {
      $('#choose-character-dialog').fadeOut(animationDuration)
      server.sendChooseCharacter($(event.target).attr("id"))
    })

    /* Game Over / Rematch */
    $('#game-over-dialog #rematch').click(function (event) {
      server.sendRematch()
      $('#game-over-dialog').fadeOut(animationDuration)
    })

    /* additional responsiveness for choice buttons */
    $('.choice').mousedown(function (event) {
      $(this).addClass("pressed")
    }).mouseup(function (event) {
      $(this).removeClass("pressed")
    }).mouseout(function (event) {
      $(this).removeClass("pressed")
    })

    /* auto-fade game-id and footer when inactive */
    $('#game-id').hover(function () {
      $(this).fadeTo(animationDuration, 1.0)
    }, function () {
      $(this).fadeTo(animationDuration, 0.5)
    })

    $('#footer').hover(function () {
      $(this).fadeTo(animationDuration, 1.0)
    }, function () {
      $(this).fadeTo(animationDuration, 0.2)
    })

    /* Keyboard shortcuts */
    $(window.document).keydown(function (event) {
      if (event.which === 65 || event.which === 186) {
        $('#player-1-deck').click()
      }
      else if (event.which === 81 || event.which === 80) {
        $('#player-1-hand').click()
      }
    })
  }
  self.init = init

  return self
}(us))
