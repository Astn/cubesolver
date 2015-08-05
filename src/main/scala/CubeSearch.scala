import scala.util.Random

/**
 * Created by astn on 8/4/15.
 */
class CubeSearch(topCenter:String, frontCenter:String) {
  def Normalize(c:RubiksCube):RubiksCube={
    def findTop(c:RubiksCube):RubiksCube={
      if(c.TopCenter.top == topCenter)
        c
      else
        findTop(c.rotate(cubeX()))
    }
    def findFront(c:RubiksCube):RubiksCube={
      if(c.FrontCenter.front == frontCenter)
        c
      else
        findFront(c.rotate(cubeY()))
    }
    findFront(findTop(c))
  }
  def Fitness(c: RubiksCube):Int = {
    // count the number of simplecubes that are on the same face as their center
    val normalizedCube = Normalize(c)
    c.FrontFacePieces.filter(s => s.front == c.FrontCenter.front) :::
    c.TopFacePieces.filter(s => s.top == c.TopCenter.top) :::
    c.BackFacePieces.filter(s => s.back == c.BackCenter.back) :::
    c.DownFacePieces.filter(s => s.down == c.DownCenter.down) :::
    c.LeftFacePieces.filter(s => s.left == c.LeftCenter.left) :::
    c.RightFacePieces.filter(s => s.right == c.RightCenter.right)
    match { case x => x.length }
  }
  lazy val lingoRaw = List(("f",front()),
                          ("fi",frontInverted()),
                          ("u",top()),
                          ("ui",topInverted()),
                          ("d",down()),
                          ("di",downInverted()),
                          ("b",back()),
                          ("bi",backInverted()),
                          ("l",left()),
                          ("li",leftInverted()),
                          ("r",right()),
                          ("ri",rightInverted()))
  lazy val lingo = lingoRaw.toMap
  lazy val lingoLen = lingoRaw.length
  lazy val lingoKeys = lingoRaw.map{case (k,v)=>k}.toArray
  def Search(c: RubiksCube): Stream[(RubiksCube,Int,List[String])] = {
    val rnd = new Random()
    def genDNA(): List[String] = {
      val rndNum = rnd.nextInt(40)
      val nums = (1 to rndNum)
      val offsets = nums.map(x => rnd.nextInt(lingoLen))
      val dna = offsets.map(x=> lingoKeys(x))
      dna.toList
    }
    def breed(parentDNA1: List[String], parentDNA2: List[String]):List[String] = {
      def chunkSize():Int = rnd.nextInt(9)+1
      def parentSelect():List[String] = rnd.nextInt(3) match { case 1 => parentDNA1 case 2 => parentDNA2 case _ => genDNA()}
      def pullFromParent(pos:Int):List[String] = {
        val parent = parentSelect()
        val dropsize = math.min(parent.length-1,pos)
        val takesize = math.min(parent.length - dropsize,chunkSize())
        parent.drop(dropsize).take(takesize)
      }
      def growChild(pos:Int,max:Int):Stream[List[String]] = {
        if(pos >= max)
          Stream.empty[List[String]]
        else{
          val chunk = pullFromParent(pos)
          chunk #:: growChild(chunk.length+pos,max)
        }
      }
      growChild(0,Math.max(parentDNA1.length+2,parentDNA2.length+2) - rnd.nextInt(5)).flatten.toList
    }
    def evalDNA(dna:List[String]):(RubiksCube,Int,List[String])={
      val algo = dna.map(op=> lingo(op))
      val solve = c.rotateSeq(algo)
      val fit = Fitness(solve)
      (solve,fit,dna)
    }
    def simplifyDNA(dna:List[String]):List[String]={
      implicit def dnaSimplifier(s : String) = {
        new {
          def replaceDNA(opp:Int,oppInverted:Int) : String = {
            val clean = s.replaceAllLiterally(Array(lingoKeys(opp),lingoKeys(opp),lingoKeys(opp)).mkString(",")+",",lingoKeys(oppInverted)+",")
              .replaceAllLiterally(Array(lingoKeys(oppInverted),lingoKeys(oppInverted),lingoKeys(oppInverted)).mkString(",")+",",lingoKeys(opp)+",")
              .replaceAllLiterally(Array(lingoKeys(oppInverted),lingoKeys(opp)).mkString(",")+",",",")
              .replaceAllLiterally(Array(lingoKeys(opp),lingoKeys(oppInverted)).mkString(",")+",",",")
            clean
          }
        }
      }
      val cleanDNA = dna.mkString(",")
        .replaceDNA(0,1)
        .replaceDNA(2,3)
        .replaceDNA(4,5)
        .replaceDNA(6,7)
        .replaceDNA(8,9)
        .replaceDNA(10,11)
        .split(',').toList.filter(x=>x.isEmpty == false)
      cleanDNA
    }
    def genGen() = (0 to 20).toStream.map(x => genDNA()).map(evalDNA).toList
    def generation(gens:Int, parents: List[(RubiksCube,Int,List[String])]): Stream[List[(RubiksCube,Int,List[String])]]={
      if(gens < 1) Stream.Empty
      else{
        val sorted = parents.sortBy{case (cube,fit,dna)=> fit}.reverse.toList
        val fit = parents.map{case (_,fit,_)=> fit }
        val fitSum:Int = fit.sum
        val fitMax = fit.max
        val avgFitness = fitSum / parents.length
        println(s"avgFit: ${avgFitness} maxFit: ${fitMax}")
        val genP1 = sorted.zip(sorted.tail).map{case ((_,_,dna1),(_,_,dna2))=> breed(dna1,dna2)}.map(evalDNA).distinct
        val genP2 = sorted.zip(sorted.tail.tail).map{case ((_,_,dna1),(_,_,dna2))=> breed(dna1,dna2)}.map(evalDNA).distinct
        val genP3 = sorted.zip(sorted.tail.tail.tail).map{case ((_,_,dna1),(_,_,dna2))=> breed(dna1,dna2)}.map(evalDNA).distinct
        val newPopulation = (sorted.take(60) ::: genP1.take(30) ::: genP2.take(30) ::: genP3.take(30) ::: genGen())
        val retPop = newPopulation.map{case (cube,fitness,dna)=>(cube,fitness,simplifyDNA(dna))}
        retPop #:: generation(gens-1,retPop)
      }
    }


    val firstGen = genGen()

     firstGen.toStream.append(generation(1000,firstGen).flatten)
  }
}
