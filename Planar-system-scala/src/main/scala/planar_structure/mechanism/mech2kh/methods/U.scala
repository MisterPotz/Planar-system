package planar_structure.mechanism.mech2kh.methods

trait U{
  def Uh4_1 : Double = 1 / U4h_1
  def U4h_1 : Double = 1 - U41_h
  def U41_h : Double = 1 / U14_h
  def U14_h : Double
  def U1h_4 : Double = 1 - U14_h
  def Uh1_4 : Double = 1 /  U1h_4
}