/**
 * Created by astn on 6/28/15.
 */


object Main {


  val algo1 = List(front(),top(),front(),left())
  def applyAlgo(c:RubiksCube, a:Seq[Rotation] ) = a.foldLeft(c)((cube,rotation)=> c.rotate(rotation))
  def main(args: Array[String]) {
      val c = SimpleCube("R","W","Y","G","B","O")
      val cube = (0 to 25).map(x=> c.clone()).toArray
      val rc = new RubiksCube(cube)
//      println("solved")
//      println(rc)
//      println("front1")
//      println(rc.rotate(front()))
//      println("front2")
//      println(rc.rotate(front()).rotate(front()))
//      println("front3")
//      println(rc.rotate(front()).rotate(front()).rotate(front()))
//      println("frontInverted")
//      println(rc.rotate(frontInverted()))
     // println("Z Rotation")
     // println(rc.rotate(cubeZ()))
//      println("X")
//      println(rc.rotate(cubeX()))
//      println(rc.rotate(cubeX(),front()))
//      println(rc.rotate(cubeX(),front(),cubeXInverted()))
//
//      println("Y")
//      println(rc.rotate(cubeY()))
//      println(rc.rotate(cubeY(),front()))
//      println(rc.rotate(cubeY(),front(),cubeYInverted()))
//
//      println("Z")
//      println(rc.rotate(cubeZ()))
//      println(rc.rotate(cubeZ(),front()))
//      println(rc.rotate(cubeZ(),front(),cubeZInverted()))

      println("start")
      println(rc)
      println("algo")
      println(rc.rotateSeq(algo1))
  }
}
