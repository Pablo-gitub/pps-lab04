package u04.monads

import u04.monads.States.State

object DrawMyNumber:
  trait GameState:
    type Game
    def initialGame(): Game
    def resetGame(): State[Game, Unit]
    def checkGuess(guess: Int): State[Game, String]
    def nop(): State[Game, Unit]

  object GameStateImpl extends GameState:

    import scala.util.Random

    opaque type Game = (Int, Int, Boolean) // (numberToGuess, attemptsLeft, endGame)

    def initialGame(): Game = (Random.nextInt(100) + 1, 10, false)

    def resetGame(): State[Game, Unit] =
      State(_ => ((Random.nextInt(100) + 1, 10, false), ()))

    def checkGuess(guess: Int): State[Game, String] = State {
      case (number, attempts, true) =>
        ((number, attempts, true), "Game over! Press reset.")
      case (number, attempts, false) =>
        if guess == number then
          ((number, attempts, true), s"You win! The number was $number.")
        else if attempts <= 1 then
          ((number, 0, true), s"You lose! The number was $number.")
        else if guess > number then
          ((number, attempts - 1, false), s"Number too high! Attempts left: ${attempts - 1}")
        else
          ((number, attempts - 1, false), s"Number too low! Attempts left: ${attempts - 1}")
    }

    def nop(): State[Game, Unit] = State(g => (g, ()))
