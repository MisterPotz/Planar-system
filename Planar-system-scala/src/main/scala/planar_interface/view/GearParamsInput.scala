package planar_interface.view

import javafx.scene.Parent

trait GearParamsInput {
  def getParent : Parent
  def clearInput() : Unit
  def blockView : Unit = ()
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
