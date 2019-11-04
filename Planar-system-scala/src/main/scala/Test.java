import javafx.application.Application;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.SplitPane;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.HBox;
import javafx.stage.Stage;
import javafx.scene.control.Label;

import planar.ConceptTest;
import planar_interface.GearParameter;
import planar_interface.view.AbstractGearViewControllerFactory;
import planar_interface.view.GearView;
import planar_interface.view.GearViewController;
import planar_interface.view.GearViewControllerFactory;
import planar_structure.mechanism.*;
import planar_structure.mechanism.Mechanism2KH;
import planar_structure.mechanism.mech2kh.*;
import scala.collection.immutable.List;

import java.io.IOException;
import planar.ConceptTest;

public class Test extends Application {
    @Override
    public void start(Stage stage) throws Exception {
        stage.setTitle("Some");
        /*FlowPane rootNode = new FlowPane();
        Scene scene = new Scene(rootNode, 300, 250);
        stage.setScene(scene);
        Label lbl = new Label("JavaFx");
        Mechanism mech = Mechanism2KH.apply("InternalInternal_CarrierInput");
        List<GearWheel> a = mech.characteristics().gearStructureCharacteristic().getGearList();
       // Node some = new GearParameter(a.apply(0), "Gear Number 0");
        rootNode.getChildren();*/
        // Загружаем корневой макет из fxml файла.
        Mechanism mech = Mechanism2KH.apply("InternalInternal_CarrierInput");
        List<GearWheel> a = mech.characteristics().gearStructureCharacteristic().getGearList();
        // Отображаем сцену, содержащую корневой макет.
        AbstractGearViewControllerFactory factory = new GearViewControllerFactory(a.apply(0));
        Parent rootLayout = factory.getParent();
        Accordion accordion = new Accordion();
        SplitPane pane = new SplitPane();
        
        GearViewController controller = factory.createGearView();
        Button btn = new Button("Click me test");
        EventHandler<ActionEvent> ev = new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent actionEvent) {
                System.out.println("S");
            }
        };
        btn.setOnAction(ev);
        Scene scene = new Scene(rootLayout);
        stage.setScene(scene);
        stage.show();
        /*for (int i =0; i<a.length(); i++){
            rootNode.getChildren().add(new Label("" + a.apply(i).holder().z()));
        }*/
    }
}
