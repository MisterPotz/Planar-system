package planar_interface.view.event_types

import planar_interface.Event

case class ArgumentGetterEvent(bundle : BundleKeeper) extends Event

//used to scrape some useful info from input
class BundleKeeper{
  var keeps_bundle : Boolean = false
  var bundle : AnyRef = _
}