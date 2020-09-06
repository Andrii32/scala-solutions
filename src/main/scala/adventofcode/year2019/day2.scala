package adventofcode.year2019

case class Index(v: Int)
case class Address(v: Int)
case class Value(v: Int)

object Index{

    implicit class IntOps(val x: Int) {
        def +(y: Index): Index = Index(x + y.v)
    }

    implicit class IndexOps(val x: Index) {
        def +(y: Int): Index = Index(x.v + y)
    }

}

object Address{

    implicit class IntOps(val x: Int) {
        def +(y: Address): Address = Address(x + y.v)
    }

    implicit class AddressOps(val x: Address) {
        def +(y: Int): Address = Address(x.v + y)
    }

}

object Value{

    implicit class IntOps(val x: Int) {
        def +(y: Value): Value = Value(x + y.v)
        def *(y: Value): Value = Value(x * y.v)
    }

    implicit class ValueOps(val x: Value) {
        def +(y: Int): Value = Value(x.v + y)
        def +(y: Value): Value = Value(x.v + y.v)
        def *(y: Int): Value = Value(x.v * y)
        def *(y: Value): Value = Value(x.v * y.v)
    }
}

import Index._, Address._, Value._


sealed trait Prog
case class Program(intcodes: Vector[Int], index: Index = Index(0)) extends Prog
case class Halt(intcodes: Vector[Int], index: Index = Index(0))    extends Prog



object Halt{

    implicit class HaltOps(val p: Halt) {
        def getValueByAddress(address: Address): Value = Value(p.intcodes(address.v))
    }

}

object Program{

    def apply(xs: List[Int]): Program =
        Program(xs.toVector)

    implicit class ProgramOps(val p: Program) {

        def getValueByAddress(address: Address): Value =
            Value(p.intcodes(address.v))

        def setValueByAddress(address: Address, value: Value): Program =
            Program(p.intcodes.updated(address.v, value.v), p.index)

        def getAddressByIndex(index: Index): Address =
            Address(p.intcodes(index.v))

        def getValueByIndex(index: Index): Value =
            getValueByAddress(getAddressByIndex(index))

        def setValueByIndex(index: Index, value: Value): Program =
            setValueByAddress(getAddressByIndex(index), value)
    }

}

object Executor{

    def code1(x: Program): Program = {
        var fstValue = x.getValueByIndex(x.index + 1)
        val secValue = x.getValueByIndex(x.index + 2)
        val p = x.setValueByIndex(x.index + 3, fstValue + secValue)
        Program(p.intcodes, index=p.index + 4)
    }

    def code2(x: Program): Program = {
        var fstValue = x.getValueByIndex(x.index + 1)
        val secValue = x.getValueByIndex(x.index + 2)
        val p = x.setValueByIndex(x.index + 3, fstValue * secValue)
        Program(p.intcodes, index=p.index + 4)
    }

    def execute(x: Program): Prog = {
        x.intcodes(x.index.v) match {
            case 1  => execute(code1(x))
            case 2  => execute(code2(x))
            case 99 => Halt(intcodes=x.intcodes, index=x.index)
        }
    }

}


object day_2{

    def task1(vals: Iterator[String]): Int = {
        val opcodes = vals.next().split(',').toList.map(_.toInt)
        // before running the program, replace position 1 with the value 12
        // and replace position 2 with the value 2.
        val program = Program(opcodes)
            .setValueByAddress(Address(1), Value(12))
            .setValueByAddress(Address(2), Value(2))
        Executor.execute(program) match {
            case p: Program => throw new NotImplementedError
            case p: Halt => p.getValueByAddress(Address(0)).v
        }
    }

}