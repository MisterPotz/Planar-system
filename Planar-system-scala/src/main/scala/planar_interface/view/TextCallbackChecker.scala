package planar_interface.view

//interface for fast setting of feedback messages for some text elements
trait TextCallbackChecker {
  val wrong_message_defined : String
  val wrong_message_undefined : String
  val setText : String => Unit
  val getText : () => String
  val checkFunction : (String) => Boolean
  def checkIfElseSet() : Boolean = {
    val a : String = getText()
    try {
      if (checkFunction(a)) {
        true
      } else {
        setText(wrong_message_defined)
        false
      }
    }catch {
      case _ : Exception =>
        setText(wrong_message_undefined)
        false
    }
  }
}

case class TextCallbackCheckerSimple(checkFunction : (String) => Boolean,
                                     getText : () => String,
                                     setText : String => Unit,
                                     wrong_message_defined : String,
                                     wrong_message_undefined : String) extends TextCallbackChecker
