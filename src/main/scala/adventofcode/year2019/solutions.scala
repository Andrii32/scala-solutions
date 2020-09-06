package adventofcode.year2019

import scala.io.Source

object solutions{

    def day1() = {
        println("DAY 1")
        println(day_1.task1(Source.fromResource("adventofcode/year2019/day1").getLines))
        println(day_1.task2(Source.fromResource("adventofcode/year2019/day1").getLines))
    }

}