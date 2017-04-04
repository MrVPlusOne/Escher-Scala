/**
  * Created by weijiayi on 04/04/2017.
  */
package object escher {

  implicit class MapOperations[A,B](map: Map[A,B]){
    def subMapOf(that: Map[A,B]): Boolean = {
      map.forall{case (v, t) => that.get(v).contains(t)}
    }
  }

  type IS[A] = IndexedSeq[A]
  val IS = IndexedSeq
}
