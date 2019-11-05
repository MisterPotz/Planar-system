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
import javafx.scene.layout.*;
import javafx.stage.Stage;
import javafx.scene.control.Label;

import planar.ConceptTest;
import planar_interface.GearParameter;
import planar_interface.view.GearGroupView.*;
import planar_interface.view.GearView.AbstractGearViewControllerFactory;
import planar_interface.view.GearListView.AbstractGearListViewControllerFactory;
import planar_interface.view.GearListView.GearListViewController;
import planar_interface.view.GearListView.GearListViewControllerFactory;
import planar_interface.view.GearView.GearView;
import planar_interface.view.GearView.GearViewController;
import planar_interface.view.GearView.GearViewControllerFactory;
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
        a.apply(0).holder().z_$eq(85);
        a.apply(1).holder().z_$eq(45);
        a.apply(2).holder().z_$eq(53);
        a.apply(3).holder().z_$eq(72);
        // Отображаем сцену, содержащую корневой макет.
        AbstractGearListViewControllerFactory factory = new GearListViewControllerFactory(a);
       // AbstractGearViewControllerFactory factory1 = new GearViewControllerFactory(a.apply(0));
       Parent rootLayout =((GearListViewController) factory.createView()).gearListView().gearsList();
       List<GearGroup> a1 = mech.characteristics().gearStructureCharacteristic().getGearGroups();
        AbstractGearGroupOnlyViewControllerFactory fac1 = new GearGroupOnlyViewControllerFactory(a1.apply(0));
        Parent root2 =((GearGroupOnlyViewController) fac1.createView()).gearGroupOnlyView().gearGroupOnlyPane();
        AbstractGearGroupFullViewControllerFactory fac2 = new GearGroupFullViewControllerFactory(a1.apply((0)));
        Parent root3 =((GearGroupFullViewController) fac2.createView()).gearGroupFullView().gearGroupPane();
        Scene scene = new Scene(root3);
        stage.setScene(scene);
        stage.show();
        /*for (int i =0; i<a.length(); i++){
            rootNode.getChildren().add(new Label("" + a.apply(i).holder().z()));
        }*/
    }
}
