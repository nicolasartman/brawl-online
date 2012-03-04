/*global 
 jquery:true,
 $:true,
 us: true,
 _: true,
 self: true,
 devel: true
*/

var us = _.noConflict();

var gameState = (function () {
  "use strict";
  var self = {}
  
  // Private vars
  var state = {}, gameID
  
  /*
   * Public - Sets the entire game state to the passed in object.
   * Better be correctly formed!
  */
  var setState = function (gameState) {
    if (!gameState) {
      console.log("Null or undefined game state passed in, using test state");
    }
    state = gameState
  }
  self.setState = setState
  
  /*
   * Public - gets a reference to the game state (for rendering)
  */
  var getState = function() {
    return state
  }
  self.getState = getState
  
  /*
   * Public - Sets the game id for this game
   * TODO: actually use this
  */
  var setGameID = function (newGameID) {
    gameID = newGameID;
  }
  self.setGameID = setGameID
  
  /*
   * Public - Gets the ID for the current game
  */
  var getGameID = function () {
    return gameID
  }
  self.getGameID = getGameID
  
  return self
}());