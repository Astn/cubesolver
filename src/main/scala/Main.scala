/**
 * Created by astn on 6/28/15.
 */


object Main {


  val algo1 =   List(front(),top(),front(),left())
  val unalgo1 = List(leftInverted(),frontInverted(),topInverted(),frontInverted())

  def main(args: Array[String]) {
      val c = SimpleCube("R","W","Y","G","B","O")
      val cube = (0 to 25).map(x=> c.clone()).toArray
      val rc = new RubiksCube(cube)
//      println("solved")


      println("start")
      println(rc)
      val cs = new CubeSearch(c.top,c.front)
      println(s"fitness: ${cs.Fitness(rc)}")
      println("normalize2")
      val mutated = rc.rotate(top(),front(),back(),right(),top())
      println(s"fitness: ${cs.Fitness(mutated)}")
      println(cs.Normalize(mutated))

      val strategy = cs.Search(mutated)
          .scanLeft((0,List(""))){case ((bestfit,bestdna),(solve,fit,dna)) => {
            if(fit>bestfit) {
              println(s" fit: ${fit} - ${dna}\n${solve}")
              (fit,dna)
            } else {
              (bestfit,bestdna)
            }
            }}.filter{case (fit,dna)=>fit>=54}.head

      println(s"This works: ${strategy._2}")

  }
}
