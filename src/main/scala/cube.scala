

/**
 * Created by astn on 6/28/15.
 */

abstract class Face()
case class FrontFace() extends Face
case class TopFace() extends Face
case class BackFace() extends Face
case class DownFace() extends Face
case class LeftFace() extends Face
case class RightFace() extends Face

abstract class SimpleRotation()
case class SimpleX() extends SimpleRotation
case class SimpleY() extends SimpleRotation
case class SimpleZ() extends SimpleRotation
case class SimpleXInverted() extends SimpleRotation
case class SimpleYInverted() extends SimpleRotation
case class SimpleZInverted() extends SimpleRotation

case class SimpleCube(front: String, top: String, down: String, left: String, right:String, back:String) {

  val _front: String = front
  val _top: String = top
  val _left: String = left
  val _right: String = right
  val _down: String = down
  val _back: String = back

  def apply(rotation: SimpleRotation) = {
    rotation match {
      case SimpleY() => SimpleCube(_right, _top, _down, _front, _back, _left)
      case SimpleYInverted() => SimpleCube(_left, _top, _down, _back, _front, _right)
      case SimpleX() => SimpleCube(_down, _front, _back, _left, _right, _top)
      case SimpleXInverted() => SimpleCube(_top, _back, _front, _left, _right, _down)
      case SimpleZ() => SimpleCube(_front,_left,_right,_down,_top,_back)
      case SimpleZInverted() => SimpleCube(_front,_right,_left,_top,_down,_back)
    }
  }

  override def clone() : SimpleCube = {
    SimpleCube(_front, _top, _down, _left, _right, _back)
  }

  def toString(face: Face): String ={
    face match {
      case FrontFace() => _front
      case TopFace() => _top
      case BackFace() => _back
      case DownFace() => _down
      case LeftFace() => _left
      case RightFace() => _right
    }
  }

  override def toString():String = {
    val space = " "
    f"${space} ${space} ${_top} ${space}\n" +
    f"${_back.padTo(8,' ')} ${_left} ${_front} ${_right}\n" +
    f"${space} ${space} ${_down} ${space}\n"
  }
}

case class Step(source:Int, dest:Int)

abstract class Rotation {
  def apply(c: RubiksCube): RubiksCube = new RubiksCube(c._cube)
}

case class cubeZ() extends Rotation {
  val frontfaceZ = List(Step(1,7),Step(7,5),Step(5,3),
                        Step(0,6),Step(6,4),Step(4,2))
  val midfaceZ = frontfaceZ.map{case Step(a,b)=>Step(a+9,b+9)}
  val backfaceZ = midfaceZ.map{case Step(a,b)=>Step(a+9,b+9)}

  val swaps = frontfaceZ ::: midfaceZ ::: backfaceZ
  override def apply(c: RubiksCube):RubiksCube = {
    val f = new RubiksCube(c._cube.map(p => p.apply(SimpleZ())))
    f.apply(swaps)
  }
}
case class cubeZInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeZ(),cubeZ(),cubeZ())
  }
}
case class cubeX() extends Rotation {
  val leftFace = List(Step(0,6), Step(6,23),Step(23,17),
    Step(7,15),Step(15,24),Step(24,9))

  val middleFace = List(Step(1,5), Step(5,22),Step(22,18),
    Step(8,14),Step(14,25),Step(25,10))

  val rightFace = List(Step(2,4), Step(4,21),Step(21,19),
    Step(3,13),Step(13,20),Step(20,11))

  val swaps = leftFace ::: middleFace ::: rightFace
  override def apply(c: RubiksCube):RubiksCube = {
    val f = new RubiksCube(c._cube.map(p=>p.apply(SimpleX())))
    f.apply(swaps)
  }
}
case class cubeXInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeX(),cubeX(),cubeX())
  }
}
case class cubeY() extends Rotation {
  val topFace = List(Step(0,2), Step(2,19), Step(19,17),
    Step(1,11), Step(11,18), Step(18,9))

  val middleFace = List(Step(7,3), Step(3,20), Step(20,24),
    Step(8,12), Step(12,25), Step(25,16))

  val bottomFace = List(Step(6,4), Step(4,21), Step(21,23),
    Step(5,13), Step(13,22), Step(22,15))
  val swaps = topFace ::: middleFace ::: bottomFace
  override def apply(c: RubiksCube):RubiksCube = {
    val f = new RubiksCube(c._cube.map(p=>p.apply(SimpleY())))
    f.apply(swaps)
  }
}
case class cubeYInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeY().apply(cubeY().apply(cubeY().apply(c)))
  }
}
case class front() extends Rotation {
  val swaps = List(Step(0,6),
    Step(6,4),
    Step(4,2),
    Step(1,7),
    Step(7,5),
    Step(5,3))
  val blocks = swaps.flatMap(x=>Seq(x.dest,x.source)).distinct.toArray
  override def apply(c: RubiksCube):RubiksCube = {
    val gah = c._cube.clone()
    // rotate the components of the cube at the block level
    for( i <- (0 to gah.length)){
      if (blocks.contains(i)) {
        gah.update(i,gah.apply(i).apply(SimpleZ()))
      }
    }
    // now move the blocks of the cube to their new positions
    val f = new RubiksCube(gah)
    f.apply(swaps)
  }
}
case class frontInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(front(),front(),front())
  }
}
case class top() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeXInverted(),front(),cubeX())
  }
}
case class topInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(top(),top(),top())
  }
}
case class down() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeZ(),front(),cubeZInverted())
  }
}
case class downInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(down(),down(),down())
  }
}
case class right() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeYInverted(),front(),cubeY())
  }
}
case class rightInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(right(),right(),right())
  }
}
case class left() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeY(),front(),cubeYInverted())
  }
}
case class leftInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(left(),left(),left())
  }
}
case class back() extends Rotation{
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(cubeY(),cubeY(),front(),cubeY(),cubeY())
  }
}
case class backInverted() extends Rotation{
  override def apply(c: RubiksCube):RubiksCube = {
    c.rotate(back(),back(),back())
  }
}

class RubiksCube (pieces: Array[SimpleCube]) {
  // front face as:
  // 0, 1, 2
  // 7, 8, 3
  // 6, 5, 4

  // center from front perspective as:
  // 9,  10, 11
  // 16, .., 12
  // 15, 14, 13

  // back from front perspective as:
  // 17, 18, 19,
  // 24, 25, 20,
  // 23, 22, 21

//                    H I J
//                    E F G
//                    B C D
//    -----------------------------
//    1 2 3 | 4 5 6 | 7 8 9 | a b c
//    d e f | g h i | j k l | m n o
//    p q r | s t u | v w x | y z A
//    -----------------------------
//                    K L M
//                    N O P
//                    Q R S
//

  lazy val TopCenter = pieces(10)
  lazy val FrontCenter = pieces(8)
  lazy val BackCenter = pieces(25)
  lazy val DownCenter = pieces(14)
  lazy val LeftCenter = pieces(16)
  lazy val RightCenter = pieces(12)

  lazy val FrontFacePieces = List(0, 1, 2, 3, 4, 5, 6, 7, 8).map(x=>pieces(x))
  lazy val TopFacePieces = List(0, 1, 2, 9, 10, 11, 17, 18, 19).map(x=>pieces(x))
  lazy val BackFacePieces = List(17, 18, 19, 20, 21, 22, 23, 24, 25).map(x=>pieces(x))
  lazy val DownFacePieces = List(4,5,6,13,14,15,21,22,23).map(x=>pieces(x))
  lazy val RightFacePieces = List(3,4,5,11,12,13,19,20,21).map(x=>pieces(x))
  lazy val LeftFacePieces = List(0,6,7,9,15,16,17,23,24).map(x=>pieces(x))

  override def toString() : String = {
    val space = " ".padTo(5, ' ')
    f"${space} ${space} ${_cube.apply(17).toString(TopFace())} ${_cube.apply(18).toString(TopFace())} ${_cube.apply(19).toString(TopFace())}\n" +
    f"${space} ${space} ${_cube.apply(9).toString(TopFace())} ${_cube.apply(10).toString(TopFace())} ${_cube.apply(11).toString(TopFace())}\n" +
    f"${space} ${space} ${_cube.apply(0).toString(TopFace())} ${_cube.apply(1).toString(TopFace())} ${_cube.apply(2).toString(TopFace())}\n" +
    f"${_cube.apply(17).toString(BackFace())} ${_cube.apply(18).toString(BackFace())} ${_cube.apply(19).toString(BackFace())} " +
    f"${_cube.apply(19).toString(LeftFace())} ${_cube.apply(9).toString(LeftFace())} ${_cube.apply(0).toString(LeftFace())} " +
    f"${_cube.apply(0).toString(FrontFace())} ${_cube.apply(1).toString(FrontFace())} ${_cube.apply(2).toString(FrontFace())} " +
      f"${_cube.apply(2).toString(RightFace())} ${_cube.apply(11).toString(RightFace())} ${_cube.apply(19).toString(RightFace())}\n" +
    f"${_cube.apply(24).toString(BackFace())} ${_cube.apply(25).toString(BackFace())} ${_cube.apply(20).toString(BackFace())} " +
    f"${_cube.apply(24).toString(LeftFace())} ${_cube.apply(16).toString(LeftFace())} ${_cube.apply(7).toString(LeftFace())} " +
    f"${_cube.apply(7).toString(FrontFace())} ${_cube.apply(8).toString(FrontFace())} ${_cube.apply(3).toString(FrontFace())} " +
     f"${_cube.apply(2).toString(RightFace())} ${_cube.apply(12).toString(RightFace())} ${_cube.apply(20).toString(RightFace())} \n" +
    f"${_cube.apply(23).toString(BackFace())} ${_cube.apply(22).toString(BackFace())} ${_cube.apply(21).toString(BackFace())} " +
    f"${_cube.apply(23).toString(LeftFace())} ${_cube.apply(15).toString(LeftFace())} ${_cube.apply(6).toString(LeftFace())} " +
    f"${_cube.apply(6).toString(FrontFace())} ${_cube.apply(5).toString(FrontFace())} ${_cube.apply(4).toString(FrontFace())} " +
     f"${_cube.apply(4).toString(RightFace())} ${_cube.apply(13).toString(RightFace())} ${_cube.apply(21).toString(RightFace())} \n" +
    f"${space} ${space} ${_cube.apply(6).toString(DownFace())} ${_cube.apply(5).toString(DownFace())} ${_cube.apply(4).toString(DownFace())}\n" +
    f"${space} ${space} ${_cube.apply(15).toString(DownFace())} ${_cube.apply(14).toString(DownFace())} ${_cube.apply(13).toString(DownFace())}\n" +
      f"${space} ${space} ${_cube.apply(23).toString(DownFace())} ${_cube.apply(22).toString(DownFace())} ${_cube.apply(21).toString(DownFace())}\n"
  }

  val _cube : Array[SimpleCube] = pieces

  def rotate(a:Rotation*) : RubiksCube = rotateSeq(a)
  def rotateSeq(a:Seq[Rotation]) : RubiksCube = a.foldLeft(this)((cube,rotation)=> rotation.apply(cube))
  override def clone() : RubiksCube = {new RubiksCube(_cube.clone())}

  def apply(swaps: Seq[Step]): RubiksCube = {
    def swap(step: Step, c: Array[SimpleCube]) = {
      val _c = c.clone()
      val tmp = _c.apply(step.dest)
      _c.update(step.dest, _c.apply(step.source))
      _c.update(step.source, tmp)
      _c
    }
    new RubiksCube(swaps.foldLeft(_cube)((cube,step)=>swap(step,cube)))
  }
}
