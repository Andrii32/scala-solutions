package adventofcode.year2019

import scala.io.Source

object solutions{

    def day1() = {
        println("DAY 1")
        println(day_1.task1(Source.fromResource("adventofcode/year2019/day1").getLines))
        println(day_1.task2(Source.fromResource("adventofcode/year2019/day1").getLines))
    }


    def day2() = {
        println("DAY 2")
        println(day_2.task1(Source.fromResource("adventofcode/year2019/day2").getLines))
        println(day_2.task2(Source.fromResource("adventofcode/year2019/day2").getLines))
    }

    def day3() = {
        println("DAY 3")
        println(day_3.task1(Source.fromResource("adventofcode/year2019/day3").getLines))
    }

}