package adventofcode.year2019

object day_1{

    def fuel(mass: Int): Int =
        ((mass.toFloat / 3).floor - 2).toInt

    def fuelSpecial(mass: Int): Int =
        Iterator.iterate(mass)(fuel).drop(1).takeWhile(_ > 0).sum

    def task1(vals: Iterator[String]): Int =
        vals.map(_.toInt).map(fuel).sum

    def task2(vals: Iterator[String]): Int =
        vals.map(_.toInt).map(fuelSpecial).sum

}