package planar_interface.view.event_types

trait EventListener{
  def callback(usefulInfo : AnyRef)
}