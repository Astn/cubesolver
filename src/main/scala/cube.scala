

/**
 * Created by astn on 6/28/15.
 */

abstract class Face()
case class Front() extends Face
case class Top() extends Face
case class Back() extends Face
case class Down() extends Face
case class Left() extends Face
case class Right() extends Face

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
      case Front() => _front
      case Top() => _top
      case Back() => _back
      case Down() => _down
      case Left() => _left
      case Right() => _right
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
  override def apply(c: RubiksCube):RubiksCube = {
    val swaps = Seq(Step(1,7),Step(7,5),Step(5,3),
      Step(0,6),Step(6,4),Step(4,2),
      Step(10,16),Step(16,14),Step(14,12),
      Step(9,15),Step(15,13),Step(13,11),
      Step(18,24),Step(24,22),Step(22,20),
      Step(17,23),Step(23,21),Step(21,19))
    val f = new RubiksCube(c._cube.map(p=>p.apply(SimpleZ())))
    f.apply(swaps)
  }
}
case class cubeZInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeZ().apply(cubeZ().apply(cubeZ().apply(c)))
  }
}
case class cubeX() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    val swaps = Seq(Step(0,6), Step(6,23),Step(23,17),
      Step(9,7), Step(7,15),Step(15,24),
      Step(1,5), Step(5,22), Step(22,18),
      Step(9,8), Step(8,14), Step(14,25),
      Step(2,4), Step(4,21), Step(21,19),
      Step(11,3), Step(3,13), Step(13,20))
    val f = new RubiksCube(c._cube.map(p=>p.apply(SimpleX())))
    f.apply(swaps)
  }
}
case class cubeXInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeX().apply(cubeX().apply(cubeX().apply(c)))
  }
}
case class cubeY() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    val swaps = Seq(Step(0,2), Step(2,19), Step(19,17),
      Step(1,11), Step(11,18), Step(18,9),
      Step(7,3), Step(3,20), Step(20,24),
      Step(8,12), Step(12,25), Step(25,16),
      Step(6,4), Step(4,21), Step(21,23),
      Step(5,13), Step(13,22), Step(22,16))

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
  override def apply(c: RubiksCube):RubiksCube = {
    val swaps = Seq(Step(0,6),
      Step(6,4),
      Step(4,2),
      Step(1,7),
      Step(7,5),
      Step(5,3))

    val blocks = swaps.flatMap(x=>Seq(x.dest,x.source)).distinct.toArray
    val gah = c._cube.clone()
    for( i <- (0 to gah.length)){
      if (blocks.contains(i)) {
        gah.update(i,gah.apply(i).apply(SimpleZ()))
      }
    }

    val f = new RubiksCube(gah)
    f.apply(swaps)
  }
}
case class frontInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    front().apply(front().apply(front().apply(c)))
  }
}
case class top() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeZ().apply(front().apply(cubeZInverted().apply(c)))
  }
}
case class topInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    top().apply(top().apply(top().apply(c)))
  }
}
case class down() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeZInverted().apply(front().apply(cubeZ().apply(c)))
  }
}
case class downInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    down().apply(down().apply(down().apply(c)))
  }
}
case class right() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeY().apply(front().apply(cubeYInverted().apply(c)))
  }
}
case class rightInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    right().apply(right().apply(right().apply(c)))
  }
}
case class left() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    cubeYInverted().apply(front().apply(cubeY().apply(c)))
  }
}
case class leftInverted() extends Rotation {
  override def apply(c: RubiksCube):RubiksCube = {
    left().apply(left().apply(left().apply(c)))
  }
}
case class back() extends Rotation{
  override def apply(c: RubiksCube):RubiksCube = {
    cubeY().apply(cubeY().apply(front().apply(cubeY().apply(cubeY().apply(c)))))
  }
}
case class backInverted() extends Rotation{
  override def apply(c: RubiksCube):RubiksCube = {
    back().apply(back().apply(back().apply(c)))
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

  override def toString() : String = {
    val space = " ".padTo(5, ' ')
    f"${space} ${space} ${_cube.apply(17).toString(Top())} ${_cube.apply(18).toString(Top())} ${_cube.apply(19).toString(Top())}\n" +
    f"${space} ${space} ${_cube.apply(9).toString(Top())} ${_cube.apply(10).toString(Top())} ${_cube.apply(11).toString(Top())}\n" +
    f"${space} ${space} ${_cube.apply(0).toString(Top())} ${_cube.apply(1).toString(Top())} ${_cube.apply(2).toString(Top())}\n" +
    f"${_cube.apply(17).toString(Back())} ${_cube.apply(18).toString(Back())} ${_cube.apply(19).toString(Back())} " +
    f"${_cube.apply(19).toString(Left())} ${_cube.apply(9).toString(Left())} ${_cube.apply(0).toString(Left())} " +
    f"${_cube.apply(0).toString(Front())} ${_cube.apply(1).toString(Front())} ${_cube.apply(2).toString(Front())} " +
      f"${_cube.apply(2).toString(Right())} ${_cube.apply(11).toString(Right())} ${_cube.apply(19).toString(Right())}\n" +
    f"${_cube.apply(24).toString(Back())} ${_cube.apply(25).toString(Back())} ${_cube.apply(20).toString(Back())} " +
    f"${_cube.apply(24).toString(Left())} ${_cube.apply(16).toString(Left())} ${_cube.apply(7).toString(Left())} " +
    f"${_cube.apply(7).toString(Front())} ${_cube.apply(8).toString(Front())} ${_cube.apply(3).toString(Front())} " +
     f"${_cube.apply(2).toString(Right())} ${_cube.apply(12).toString(Right())} ${_cube.apply(20).toString(Right())} \n" +
    f"${_cube.apply(23).toString(Back())} ${_cube.apply(22).toString(Back())} ${_cube.apply(21).toString(Back())} " +
    f"${_cube.apply(23).toString(Left())} ${_cube.apply(15).toString(Left())} ${_cube.apply(6).toString(Left())} " +
    f"${_cube.apply(6).toString(Front())} ${_cube.apply(5).toString(Front())} ${_cube.apply(4).toString(Front())} " +
     f"${_cube.apply(4).toString(Right())} ${_cube.apply(13).toString(Right())} ${_cube.apply(21).toString(Right())} \n" +
    f"${space} ${space} ${_cube.apply(6).toString(Down())} ${_cube.apply(5).toString(Down())} ${_cube.apply(4).toString(Down())}\n" +
    f"${space} ${space} ${_cube.apply(15).toString(Down())} ${_cube.apply(14).toString(Down())} ${_cube.apply(13).toString(Down())}\n" +
      f"${space} ${space} ${_cube.apply(23).toString(Down())} ${_cube.apply(22).toString(Down())} ${_cube.apply(21).toString(Down())}\n"
  }

  val _cube : Array[SimpleCube] = pieces

  def rotate(rotation: Rotation) : RubiksCube = {
    rotation.apply(this)
  }
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
