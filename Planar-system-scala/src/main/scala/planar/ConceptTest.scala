package planar

import javafx.fxml.FXML
import javafx.scene.control.Label

class ConceptTest {
  @FXML
  private var testingLabelLbl : Label = _
  def getTestingLabelLbl() : Label = this.testingLabelLbl
  def setTestingLabelLbl(label: Label) : Unit = {this.testingLabelLbl = label}
  def setText() : Unit ={
    testingLabelLbl.setText("WHAT THE FUCK OMG")
  }
}
