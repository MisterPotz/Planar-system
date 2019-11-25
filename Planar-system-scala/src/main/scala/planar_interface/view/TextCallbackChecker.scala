package planar_interface.view

//interface for fast setting of feedback messages for some text elements
trait TextCallbackChecker {
  val can_use_prompt_as_input : Boolean
  val wrong_message_defined : String
  val wrong_message_undefined : String
  val setText : String => Unit
  val getText : () => String
  val getPromptText : () => String
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
        if (a == "") {
          if (can_use_prompt_as_input){
            setText(getPromptText())
            true
          }
          else
            false
        } else {
          setText(wrong_message_undefined)
          false
        }
    }
  }
}

case class TextCallbackCheckerSimple(checkFunction : (String) => Boolean,
                                     getText : () => String,
                                     setText : String => Unit,
                                     wrong_message_defined : String,
                                     wrong_message_undefined : String,
                                     can_use_prompt_as_input: Boolean = false,
                                     getPromptText : () => String = () => "") extends TextCallbackChecker
