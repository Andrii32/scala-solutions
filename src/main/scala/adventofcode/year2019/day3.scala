package adventofcode.year2019

import cats._
import cats.implicits._


sealed trait Vec{
    def distance: Int
}

case class U(distance: Int) extends Vec
case class D(distance: Int) extends Vec
case class L(distance: Int) extends Vec
case class R(distance: Int) extends Vec


object Vec{

    def apply(x: String): Vec =
        x match {
            case x if x.startsWith("U") => U(x.tail.toInt)
            case x if x.startsWith("D") => D(x.tail.toInt)
            case x if x.startsWith("L") => L(x.tail.toInt)
            case x if x.startsWith("R") => R(x.tail.toInt)
            case _ => throw new NotImplementedError
        }
}

sealed trait Intersection
case class Point(x: Int, y: Int) extends Intersection
sealed trait Segment extends Intersection {
    def toPoints: List[Point]
    def p1: Point
    def p2: Point
    def length: Int
    def v: Int
    def vs: List[Int]
    def split(p: Point): Tuple2[Segment, Segment]
}

case class SegmentVer(x: Int, ys: Tuple2[Int, Int])  extends Segment{
    def toPoints: List[Point] = ys.toList.map(y => Point(x, y))
    def p1: Point = Point(x=x, y=ys._1)
    def p2: Point = Point(x=x, y=ys._2)
    def length: Int = (Math.max(ys._1, ys._2) - Math.min(ys._1, ys._2)).abs
    def v: Int = x
    def vs: List[Int] = ys.toList
    def split(p: Point): Tuple2[SegmentVer, SegmentVer] =
        Tuple2(SegmentVer(x, Tuple2(ys._1, p.y)), SegmentVer(x, Tuple2(p.y, ys._2)))
}

case class SegmentHor(y: Int, xs: Tuple2[Int, Int])  extends Segment{
    def toPoints: List[Point] = xs.toList.map(x => Point(x, y))
    def p1: Point = Point(x=xs._1, y=y)
    def p2: Point = Point(x=xs._2, y=y)
    def length: Int = (Math.max(xs._1, xs._2) - Math.min(xs._1, xs._2)).abs
    def v: Int = y
    def vs: List[Int] = xs.toList
    def split(p: Point): Tuple2[SegmentHor, SegmentHor] =
        Tuple2(SegmentHor(y, Tuple2(xs._1, p.x)), SegmentHor(y, Tuple2(p.x, xs._2)))

}

case class Step(point: Point, n: Int)


object Point{
    def move(p: Point, v: Vec): Point =
        v match {
            case U(d) => Point(p.x, p.y + d)
            case D(d) => Point(p.x, p.y - d)
            case R(d) => Point(p.x + d, p.y)
            case L(d) => Point(p.x - d, p.y)
        }

    def manhattanDistance(fst: Point, sec: Point): Int =
        (fst.x - sec.x).abs + (fst.y - sec.y).abs
}

object Segment{

    def apply(a: Point, b:Point): Segment = (a, b) match {
        case (Point(x1, y1), Point(x2, y2)) if x1 == x2 =>
            SegmentVer(x=x1, Tuple2(y1, y2))
        case (Point(x1, y1), Point(x2, y2)) if y1 == y2 =>
            SegmentHor(y=y1, Tuple2(x1, x2))
        case _ => throw new IllegalArgumentException()
    }

    private def intersectionParallel(vs1: Tuple2[Int, Int], vs2: Tuple2[Int, Int]): Option[Tuple2[Int, Int]] =
        if ((vs1._2 >= vs2._1) || (vs1._1 >= vs2._2)) {
            Some(Tuple2(List(vs1._1, vs2._1).max, List(vs1._2, vs2._2).min))
        } else {
            None
        }

    private def intersectionPerpendicular(y: Int, xs: Tuple2[Int, Int], x:Int, ys: Tuple2[Int, Int]): Option[Point] =
        if ((List(xs._1, xs._2).min <= x) && (x <= List(xs._1, xs._2).max) && (List(ys._1, ys._2).min <= y) && (y <= List(ys._1, ys._2).max)) {
            Some(Point(x, y))
        } else {
            None
        }

    def intersection(s1: Segment, s2: Segment): Option[Intersection] = (s1, s2) match {
        case (SegmentHor(y1, xs1),   SegmentHor(y2, xs2)) if y1 == y2   => {
            intersectionParallel(xs1, xs2)
                .map(vs => SegmentHor(y=y1, vs))
        }
        case (SegmentVer(x1, ys1),   SegmentVer(x2, ys2)) if x1 == x2   => {
            intersectionParallel(ys1, ys2)
                .map(vs => SegmentHor(y=x1, vs))
        }
        case (SegmentHor(y, xs), SegmentVer(x, ys)) => {
            intersectionPerpendicular(y, xs, x, ys)
        }
        case (SegmentVer(x, ys), SegmentHor(y, xs)) => {
            intersectionPerpendicular(y, xs, x, ys)
        }
        case _ => None
    }
}


object day_3{

    def task1(vals: Iterator[String]): Int = {
        val fst :: sec :: Nil = vals.take(2)
            .map(_.split(','))
            .map(_.toList.map(Vec.apply)).toList

        val fstPathPoints = fst.scanLeft(Point(0, 0))((p: Point, d: Vec) => Point.move(p, d))
        val secPathPoints = sec.scanLeft(Point(0, 0))((p: Point, d: Vec) => Point.move(p, d))
        val fstPathSegments = fstPathPoints.zip(fstPathPoints.drop(1)).map{case (p1, p2) => Segment(p1, p2)}
        val secPathSegments = secPathPoints.zip(secPathPoints.drop(1)).map{case (p1, p2) => Segment(p1, p2)}

        (for { x <- fstPathSegments; y <- secPathSegments } yield (x, y)).drop(1)
            .map{case (x, y) => Segment.intersection(x, y)}
            .flatten
            .flatMap(x => x match {
                case p: Point   => List(p)
                case s: Segment => s.toPoints
            })
            .map(p => Point.manhattanDistance(Point(0 ,0), p))
            .min
    }

    def task2(vals: Iterator[String]): Int = {
        val fst :: sec :: Nil = vals.take(2)
            .map(_.split(','))
            .map(_.toList.map(Vec.apply)).toList

        val fstPathSegments = fst.scanLeft(
            Tuple2(Segment(Point(0, 0), Point(0, 0)), 0)
        )(
            (s: Tuple2[Segment, Int], d: Vec) => {
                val newSegment = Segment(s._1.p2, Point.move(s._1.p2, d))
                Tuple2(newSegment, s._2 + newSegment.length)
            }
        )

        val secPathSegments = sec.scanLeft(
            Tuple2(Segment(Point(0, 0), Point(0, 0)), 0)
        )(
            (s: Tuple2[Segment, Int], d: Vec) => {
                val newSegment = Segment(s._1.p2, Point.move(s._1.p2, d))
                Tuple2(newSegment, s._2 + newSegment.length)
            }
        )

        (for { x <- fstPathSegments; y <- secPathSegments } yield (x, y))
            .mapFilter{case ((s1, d1), (s2, d2)) => Segment.intersection(s1, s2)
                .map(intersection => ((s1, d1), (s2, d2), intersection))}
            .filter{case ((s1, _), (s2, _), _) => (List(s1.v, s2.v) ::: s1.vs ::: s2.vs).forall(_ != 0)}
            .map{case ((s1, d1), (s2, d2), i) => i match {
                    case p: Point   => {
                        d1 - s1.split(p)._2.length + d2 - s2.split(p)._2.length
                    }
                    case s: Segment => throw new NotImplementedError()}}
            .min

    }

}
