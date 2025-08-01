package chapterOne.exercises

import chapterOne.StraightLineProgram
import chapterOne.StraightLineProgram.{Exp, ExpList, Stm}

import scala.annotation.tailrec

object MaxArgs {

  // gives maximum number of arguments of any print statement
  def maxArgs(stm: Stm): Int = {

    def maxArgsRec(stm: Stm, current: Int): Int =
      stm match {
        case StraightLineProgram.CompoundStm(left, right) =>
          val leftDepth = maxArgsRec(left, current)
          val rightDepth = maxArgsRec(right, current)
          if (rightDepth > leftDepth && rightDepth > current) rightDepth
          else if (leftDepth > rightDepth && leftDepth > current) leftDepth
          else current
        case StraightLineProgram.AssignStm(id, exp) =>
          val expDepth = checkExpDepth(exp)
          if (expDepth > current) expDepth
          else current
        case StraightLineProgram.PrintStm(list) =>
          val listDepth = checkListDepth(list)
          if (listDepth > current) listDepth
          else current
      }

    def checkExpDepth(exp: Exp, current: Int = 0): Int =
      exp match {
        case StraightLineProgram.IdExp(id)  => current
        case StraightLineProgram.NumExp(num) => current
        case StraightLineProgram.OpExp(left, op, right) => current
        case StraightLineProgram.EseqExp(stm, exp) =>
          val stmDepth = maxArgsRec(stm, current)
          val expDepth = checkExpDepth(exp, current)
          if (stmDepth > expDepth) stmDepth
          else expDepth
      }

    @tailrec
    def checkListDepth(list: ExpList, current: Int = 0): Int =
      list match {
        case StraightLineProgram.PairExpList(exp, list) => checkListDepth(list, current + 1)
        case StraightLineProgram.LastExpList(exp) => current + 1
      }

    maxArgsRec(stm, 0)
  }

}
