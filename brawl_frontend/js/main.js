/*global
 jquery:true,
 $:true,
 us: true,
 view: true,
 document: true,
 WebSocket: true,
 MozWebSocket: true,
 gameState: true,
 window: true
*/

$(document).ready(function ($) {
  "use strict";
  var socket
  
  /* debug mode */
  var debug = window.location.href.indexOf("debug") !== -1 ? true : false
  
  try {
    socket = new WebSocket("ws://" + window.location.host + ":8080/play")
  } catch (e) {
    socket = new MozWebSocket("ws://" + window.location.host + ":8080/play")
  }

  // Initialize view actions so the view can send messages back through the socket
  view.init({
    sendConnect: function (gameID) {
      gameState.setGameID(gameID)
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
        gameID: gameState.getGameID()
      }))
    },
    sendCardMove: function (from, to, baseID) {
      socket.send(JSON.stringify({
        gameID:       gameState.getGameID(),
        messageType:  "card_move",
        from:         from,
        to:           to,
        toBase:       baseID
      }))
    }
  })

  socket.onopen = function() {
    view.showChooseGameDialog()
  }
  socket.onmessage = function(msg) {
    var message = JSON.parse(msg.data);
    if (debug) { console.log(message) }

    if (message.messageType === "connected") {
      view.displayGameID()
      view.clearNotification()
      view.showChoosePlayerTypeDialog() // triggers a join event after prompting them
    }
    else if (message.messageType === "joined") {
      view.clearNotification()
      view.showNotification("Waiting for other player...")
      console.log("Joined successfully, waiting for other player")
    }
    else if (message.messageType === "started") {
      view.clearNotification()
      
      // Reveal the playing area
      view.showPlayArea()
      view.setPlayerDeckNames(message.data.player1Deck, message.data.player2Deck)

      // Render the initial game and go!
      gameState.setState(message.data.gameState)
      view.render();
    }
    else if (message.messageType === "game_state") {
      // Update the game state and re-render
      gameState.setState(message.data.gameState)
      view.render();
    }
    else if (message.messageType === "game_over") {
      alert(message.data.winner + " wins!")
      socket.send(JSON.stringify({
        messageType: "new_game",
        gameID: gameState.getGameID()
      }))
    }
    else if (message.messageType === "pause") {
      view.showNotification("Other player disconnected, waiting for rejoin...")
    }
    else if (message.messageType === "resume") {
      view.clearNotification()
    }
    // Recoverable errors
    else if (message.messageType === "error" && message.data.errorType === "join_failed") {
      view.showChoosePlayerTypeDialog()
    }
    else if (message.messageType === "error" && message.data.errorType === "game_not_found") {
      view.showChooseGameDialog()
    }
    else {
      console.log("Invalid/error Message Received: ")
      console.log(message)
    }
    if (message.messageType === "error") {
      alert("Error: " + message.data.errorMessage)
    }
  }
  socket.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  }

});
