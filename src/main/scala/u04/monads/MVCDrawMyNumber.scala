package u04.monads

import u04.monads.DrawMyNumber.GameStateImpl.*

@main def runMVCDrawMyNumber =
  import Monads.*, Monad.*, States.*, State.*, WindowStateImpl.*
  import u03.extensionmethods.Streams.*

  def mv[SM, SV, AM, AV](m1: State[SM,AM], f: AM => State[SV,AV]): State[(SM,SV), AV] =
    State: (sm, sv) =>
      val (sm2, am) = m1.run(sm)
      val (sv2, av) = f(am).run(sv)
      ((sm2, sv2), av)

  def windowCreation(str: String): State[Window, Stream[String]] = for
    _ <- setSize(300, 300)
    _ <- addButton(text = "reset", name = "ResetButton")
    _ <- addTextField(name  = "TextField")
    _ <- addButton(text = "check", name = "CheckButton")
    _ <- addButton(text = "quit", name = "QuitButton")
    _ <- addLabel(text = str, name = "Label1")
    _ <- show()
    events <- eventStream()
  yield events


  def handleEvent(e: String): State[(Game, Window), Unit] = e match
    case "ResetButton" => mv(resetGame(), _ => toLabel("New Game! Guess a number.", "Label1"))
    case "CheckButton" =>
      mv(nop(), _ => getTextFieldContent("TextField")).flatMap { txt =>
        txt.toIntOption match
          case Some(value) =>
            mv(checkGuess(value), msg => toLabel(msg, "Label1"))
          case None =>
            mv(nop(), _ => toLabel("Not valid value!", "Label1"))
      }
    case "QuitButton" => mv(nop(), _ => exec(sys.exit()))

  val controller = for
    events <- mv(nop(), _ => windowCreation("Guess a number (1-100)!"))
    _ <- seqN(events.map(handleEvent))
  yield ()



  controller.run((initialGame(), initialWindow))