/**
 * Created by astn on 6/28/15.
 */


object Main {
  def main(args: Array[String]) {
      val c = SimpleCube("R","W","Y","G","B","O")
      val cube = (0 to 25).map(x=> c.clone()).toArray
      val rc = new RubiksCube(cube)
      println("solved")
      println(rc)
      println("front1")
      println(rc.rotate(front()))
      println("front2")
      println(rc.rotate(front()).rotate(front()))
      println("front3")
      println(rc.rotate(front()).rotate(front()).rotate(front()))
      println("frontInverted")
      println(rc.rotate(frontInverted()))
  }
}
