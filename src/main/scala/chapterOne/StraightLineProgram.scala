package chapterOne

import chapterOne.StraightLineProgram.Binop.*
import chapterOne.exercises.MaxArgs

object StraightLineProgram extends App {

  sealed trait Stm
  case class CompoundStm(left: Stm, right: Stm) extends Stm
  case class AssignStm(id: String, exp: Exp) extends Stm
  case class PrintStm(list: ExpList) extends Stm

  sealed trait Exp
  case class IdExp(id: String) extends Exp
  case class NumExp(num: Int) extends Exp
  case class OpExp(left: Exp, op: Binop, right: Exp) extends Exp
  case class EseqExp(stm: Stm, exp: Exp) extends Exp

  sealed trait ExpList
  case class PairExpList(exp: Exp, list: ExpList) extends ExpList
  case class LastExpList(exp: Exp) extends ExpList

  enum Binop:
    case +, -, *, /


  // representing a := 5 + 3; b := (print(a, a - 1), 10 * a); print(b)
  val program =
    CompoundStm(
      AssignStm("a", OpExp(NumExp(5), +, NumExp(3))),
      CompoundStm(
        AssignStm(
          "b",
          EseqExp(
            PrintStm(
              PairExpList(IdExp("a"), LastExpList(OpExp(IdExp("a"), -, NumExp(1))))
            ),
            OpExp(NumExp(10), *, IdExp("a"))
          )
        ),
        PrintStm(
          LastExpList(IdExp("b"))
        )
      )
    )

  println(MaxArgs.maxArgs(program))

}
