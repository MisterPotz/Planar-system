package planar_interface.view

import javafx.scene.Parent

trait GearParamsInput {
  def getParent : Parent
  def clearInput() : Unit
  def blockView(): Unit = {
    getParent.setDisable(true)
  }
  def isBlocked: Boolean = {
    getParent.isDisabled
  }
  def unblockView(): Unit = {
    getParent.setDisable(false)
  }

  def blockView(boolean: Boolean) : Unit={
    if (boolean)
      blockView()
    else
      unblockView()

  }
  //checks the input
  def checkInput : Boolean
  def performSideEffect() : Unit
  def performIfChecked : Boolean = {
    val check = checkInput
    if (checkInput)
      performSideEffect()
    check
  }
  //can be used to obtain some input from the textfields, etc. and pass it to the calling function
  //by default its null, but we may need it
  def getUsefulObject : AnyRef = null

}
