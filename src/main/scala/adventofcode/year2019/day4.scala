package adventofcode.year2019

import cats._
import cats.implicits._
import cats.data.{NonEmptyChain, NonEmptyList}


object day_4{

    /**
      * Permuatations with repetition:
      *     Formula: n^r
      *     Example:
      *         - choosing 2 out of 10:
      *             for (i<- 0 until 10; j <- 0 until 10) yield (i,j)
      *             10^2 = 100
      *
      * Permuatations without repetition:
      *                 n!
      *     Formula: --------
      *              (n - r )!
      *     Example:
      *         - choosing 2 out of 10:
      *             for (i<- 0 until 10; j <- 0 until 10 if i != j) yield (i,j)
      *             10!/(10 - 2)! = 90
      *
      * Combinations with repetition:
      *              (r + n - 1)!
      *     Formula: ------------
      *               r!(n - 1)!
      *     Example:
      *         - choosing 2 out of 5:
      *             List.fill(2)((0 until 5).toList).flatten.combinations(2).toList
      *
      * Combinations without repetition:
      *                 n!
      *     Formula: --------
      *              r!(n - r )!
      *     Example:
      *         - choosing 2 out of 10:
      *             (for (i<- 0 until 10; j <- 0 until 10 if i != j) yield Set(i,j)).toSet
      *             (0 until 10).toList.combinations(2).toList
      *             10!/10!(10 - 2)! = 45
      */

    import scala.language.postfixOps

    def permWithRepetition(n: Int, r: Int) = Math.pow(n, r)

    def task1(vals: Iterator[String]): Int = {
        val range = vals.next.split('-').toList
        (range.head.toInt until range.last.toInt)
            .map(number => number.toString.toList)
            // Going from left to right, the digits never decrease;
            // they only ever increase or stay the same (like 111123 or 135679).
            .filter(number => number.sliding(2).forall{case List(x, y) => x <= y})
            // Two adjacent digits are the same (like 22 in 122345).
            .filter(number => number.sliding(2).exists{case List(x, y) => x == y})
            .size
    }

    def task2(vals: Iterator[String]): Int = {
        val range = vals.next.split('-').toList
        (range.head.toInt until range.last.toInt)
            .map(number => number.toString.toList)
            // Going from left to right, the digits never decrease;
            // they only ever increase or stay the same (like 111123 or 135679).
            .filter(number => number.sliding(2).forall{case List(x, y) => x <= y})
            // Two adjacent digits are the same (like 22 in 122345).
            // The two adjacent matching digits are not part of a larger group of matching digits
            .filter(number => number.groupByNel(x => x).values.map(_.toList).exists{
                case List(x, y) => x == y
                case _ => false
            })
            .size

    }

}
