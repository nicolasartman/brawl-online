/*global
 jquery:true,
 $:true,
 us: true,
 gameState: true,
 window: true */

var view = (function () {
  "use strict";
  var self = {}

  /*
   * Public - Renders the entire game state
  */
  var render = function() {
    var state = gameState.getState()

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
          "class": "base card",
          html: ("<div class='cardLabelMiddle'>" + "base" + modifiers + "</div>")
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
      for (i = 0; i < laneData.P1.length; i++) {
        // css fix for cards so they stack going upwards instead of down
        laneView.append(getViewForCard(laneData.P1[i]).attr({
          location: 'base_p1',
          baseId: laneData.id
        }).css('margin-top', -180-i*30))
      }
      // Bottom stack
      for (i = 0; i < laneData.P2.length; i++) {
        laneView.append(getViewForCard(laneData.P2[i]).attr({
          location: 'base_p2',
          baseId: laneData.id
        }).css('margin-top', 60+i*30))
      }

      return laneView
    }

    var renderPlayerHandAndDiscard = function (playerName) {
      var playerData = state[playerName]

      // Render the cards in their hand and discard
      us.each(["Hand", "Discard"], function (position) {
        // Clear old card color
        $("#" + playerName + position).removeClass('red blue green none')
        if (playerData[position.toLowerCase()]) {
          // Set new color
          $("#" + playerName + position).addClass(playerData[position.toLowerCase()].color)
          // Set card type label
          $("#" + playerName + position).text(playerData[position.toLowerCase()].cardType)
        } else {
          // Clear card type label
          $("#" + playerName + position).text(position)
        }
      })
    }

    us.each(["P1", "P2"], function (playerName) {
      renderPlayerHandAndDiscard(playerName)
    })

    // TODO: rename all this crap so it makes some semblance of sense!
    // For each lane
    $('.lane').html("");
    us.each(state.bases, function (laneData, currentLaneNumber) {
      $('.lane').eq(currentLaneNumber).html(getViewsForLane(laneData))
    })
  }
  self.render = render

  /*
   * Public - Initializes the view
  */
  var init = function (callbacks) {
    // Play on top/bottom of lane when the lane or the lane itself is clicked
    $('.lane').click(function (event) {
      // Ignore clicked base events and allow them to keep bubbling up
      if (!$(event.target).hasClass("base")) {
        console.log("Clicked lane")

        callbacks.sendCardMove("hand",
          ((event.pageY - $(this).offset().top < $(this).height() / 2) ? "base_p1" : "base_p2"),
          ($(this).find('div.base').first().attr('baseid')))
        event.stopPropagation()
      }
    })
    $('#play-area').click(function (event) {
      // Catch bubbled up card events
      if ($(event.target).hasClass('card')) {
        callbacks.sendCardMove($(event.target).attr("fromLocation"), 
                     $(event.target).attr("toLocation"),
                     $(event.target).attr("baseid"))
      }
      // Else attempt to play a new base on the left or right
      else {
        var to = (event.pageX - $(this).offset().left < $(this).width() / 2) ? "base_left" : "base_right"
        callbacks.sendCardMove("hand", to)
      }
    })
    
    // Choose player type dialog
    $('#choose-player-type-dialog .player-type-choice').click(function (event) {
      $('#choose-player-type-dialog').fadeOut(200)
      callbacks.sendJoin($(event.target).attr("choice"))
    })
    
    // Choose new game or join existing game dialog
    $('#new-game').click(function (event) {
      $('#choose-game-dialog').fadeOut(200)
      $.get('http://' + window.location.host + '/brawl/new_game', function (data, textStatus, xhr) {
        var gameID = JSON.parse(data)["GameId:"]

        // TODO: fix key so it's gameID
        callbacks.sendConnect(gameID)
      })
    })
    $('#existing-game').click(function (event) {
      var gameID = $('#existing-game-container input').val()
      $('#choose-game-dialog').fadeOut(200)
      callbacks.sendConnect(gameID)
    })
    $('#existing-game-container input').focus(function(event) {
      $(this).val("")
    })
  }
  self.init = init

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
  var displayGameID = function () {
    $('#game-id').text("Game ID: " + gameState.getGameID() + " ")
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
  var showChoosePlayerTypeDialog = function () {
    // prompt for player type
    $('#connecting-notification').slideUp(200)
    $('#choose-player-type-dialog').fadeIn(200);
  }
  self.showChoosePlayerTypeDialog = showChoosePlayerTypeDialog

  /*
   * Public - Shows the play area to the user
  */
  var showPlayArea = function () {
    $('#play-area').show(500)
  }
  self.showPlayArea = showPlayArea

  return self
}())