/*global
 jquery:true,
 $:true,
 us: true,
 view: true,
 document: true,
 WebSocket: true,
 MozWebSocket: true,
 gameState: true,
 window: true,
 setTimeout: true,
 clearTimeout: true
*/

$(document).ready(function ($) {
  "use strict";
  var socket
  
  /* the only "state" var */
  var gameID
  
  /* debug mode */
  var debug = window.location.href.indexOf("debug") !== -1 ? true : false
  
  // In case the server never connects, show a timeout message to inform the user
  // For the purposes of such a fast-paced game, 5 seconds will be considered 'never'
  var connectionTimeout = setTimeout(function () {
    view.showFailedToConnectMessage()
  }, 5000)
  
  // Connect with chrome/safari
  try {
    // socket = new WebSocket("ws://brawlwith.us/play")
    socket = new WebSocket("ws://" + window.location.host + "/play")
  } catch (e) {
    // Connect with firefox
    try {
      socket = new MozWebSocket("ws://" + window.location.host + ":8080/play")      
    } catch (ex) {
      clearTimeout(connectionTimeout)
      view.showWebsocketsNotSupportedMessage()
    }
  }

  // Initialize view actions so the view can send messages back through the socket
  view.init({
    sendConnect: function (newGameID) {
      gameID = newGameID
      view.showNotification('connecting')
      socket.send(JSON.stringify({
        messageType: "connect",
        gameID: gameID
      }))
    },
    sendJoin: function (playerType, playerCharacter) {
      view.showNotification('joining game...')
      socket.send(JSON.stringify({
        messageType: "join",
        playerType: playerType,
        deck: playerCharacter,
        gameID: gameID
      }))
    },
    sendChooseCharacter: function (character) {
      view.showNotification('choosing character...')
      socket.send(JSON.stringify({
        messageType:  "choose_character",
        gameID:       gameID,
        character:    character
      }))
    },
    sendCardMove: function (from, to, baseID) {
      socket.send(JSON.stringify({
        gameID:       gameID,
        messageType:  "card_move",
        from:         from,
        to:           to,
        toBase:       baseID
      }))
    },
    sendGetGameState: function () {
      socket.send(JSON.stringify({
        gameID: gameID,
        messageType: "get_game_state"
      }))
    },
    sendRematch: function () {
      view.showNotification('waiting for other player...')
      socket.send(JSON.stringify({
        gameID: gameID,
        messageType: "rematch"
      }))
    }
  })

  socket.onopen = function() {
    clearTimeout(connectionTimeout)
    view.showChooseGameDialog()
  }
  socket.onmessage = function(msg) {
    var message = JSON.parse(msg.data);
    if (debug) { console.log(message) }

    if (message.messageType === "connected") {
      view.clearNotification()
      view.showChoosePlayerTypeDialog({ player1: message.data.player1, player2: message.data.player2 })
    }
    else if (message.messageType === "joined") {
      view.clearNotification()
      if (!message.data.started) {
        view.showChooseCharacterDialog()        
      }
    }
    else if (message.messageType === "character_chosen") {
      view.clearNotification()
      view.showNotification("Waiting for other player...")
    }
    else if (message.messageType === "started") {
      view.clearNotification()
      
      // Reveal the playing area
      view.showPlayArea()
      view.setPlayerDeckNames(message.data.player1Deck, message.data.player2Deck)

      // Render the initial game and go!
      view.update(message.data.gameState)
    }
    else if (message.messageType === "game_state") {
      // Update the game state and re-render
      view.update(message.data.gameState)
    }
    else if (message.messageType === "game_over") {
      view.showGameOverDialog(message.data.winner + " wins!")
    }
    else if (message.messageType === "pause") {
      view.showNotification("Other player disconnected, waiting for rejoin...")
    }
    else if (message.messageType === "resume") {
      view.clearNotification()
    }
    // Recoverable errors
    else if (message.messageType === "error" && message.data.errorType === "join_failed") {
      view.showChoosePlayerTypeDialog({ player1: true, player2: true })
    }
    else if (message.messageType === "error" && message.data.errorType === "game_not_found") {
      view.showChooseGameDialog()
    }
    else {
      console.log("Invalid/error Message Received: ")
      console.log(message)
    }
    if (message.messageType === "error") {
      view.showNotification("Error: " + message.data.errorMessage)
      setTimeout(function () {
        if (view.getNotificationMessage() == "Error: " + message.data.errorMessage) {
          view.clearNotification()          
        }
      }, 3000)
    }
  }
  socket.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  }

});
