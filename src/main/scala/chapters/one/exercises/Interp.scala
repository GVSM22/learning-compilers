package chapters.one.exercises

import chapters.one.StraightLineProgram
import chapters.one.StraightLineProgram.{Binop, Exp, ExpList, Stm}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Interp {

  // interpret a straight line program
  def interp(stm: Stm): Unit = {
    interpret(stm, Map.empty[String, Int]) match {
      case Failure(exception) => println(exception.getMessage)
      case Success(value) => ()
    }
  }

  private def interpret(stm: Stm, variables: Map[String, Int]): Try[Map[String, Int]] = {
    stm match {
      case StraightLineProgram.CompoundStm(left, right) =>
        interpret(left, variables).flatMap(interpret(right, _))
      case StraightLineProgram.AssignStm(id, exp) =>
        interpretExp(exp, variables).map(value => variables.updated(id, value))
      case StraightLineProgram.PrintStm(list) =>
        interpretListExp(list, variables).map { list =>
          list.foreach { e => print(e); print(" ") }
          variables
        }
    }
  }

  @tailrec
  private def interpretListExp(list: ExpList, variables: Map[String, Int], expressions: Seq[Int] = Seq.empty): Try[Seq[Int]] =
    list match {
      case StraightLineProgram.PairExpList(exp, list) =>
        interpretExp(exp, variables) match {
          case Success(value) => interpretListExp(list, variables, Seq(value))
          case f => f.asInstanceOf[Try[Seq[Int]]]
        }
      case StraightLineProgram.LastExpList(exp) =>
        interpretExp(exp, variables).map(expressions :+ _)
    }

  private def interpretExp(exp: Exp, variables: Map[String, Int]): Try[Int] =
    exp match {
      case StraightLineProgram.IdExp(id) => variables.get(id) match {
        case Some(value) => Try(value)
        case None => Failure(Throwable(s"reference '$id' could not be found"))
      }
      case StraightLineProgram.NumExp(num) => Success(num)
      case StraightLineProgram.OpExp(left, op, right) =>
        val leftV = interpretExp(left, variables)
        val rightV = interpretExp(right, variables)
        (leftV, rightV) match {
          case (Success(l), Success(r)) => interpretOp(op, l, r)
          case (Failure(t1), Failure(t2)) => Failure(Throwable(t1.getMessage + "\n" + t2.getMessage))
          case (f: Failure[Any], _) => f
          case (_, f: Failure[Any]) => f
        }

      case StraightLineProgram.EseqExp(stm, exp) =>
        interpret(stm, variables).flatMap(interpretExp(exp, _))
    }

  private def interpretOp(op: Binop, left: Int, right: Int): Try[Int] =
    op match {
      case StraightLineProgram.Binop.+ => Success(left + right)
      case StraightLineProgram.Binop.- => Success(left - right)
      case StraightLineProgram.Binop.* => Success(left * right)
      case StraightLineProgram.Binop./ => Try(left / right)
    }
}
