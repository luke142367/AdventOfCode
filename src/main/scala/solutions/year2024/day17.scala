package solutions.year2024

import utils.Day
import utils.FileHandler.readFile
import utils.Year.Year24

import scala.collection.mutable.PriorityQueue as PQ
import scala.annotation.tailrec

case class Registers(a: Long, b: Long, c: Long)

abstract class Instr {
  val operand: Int

  def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int)

  def combo(registers: Registers): Long = operand match
    case 0 | 1 | 2 | 3 => operand
    case 4 => registers.a
    case 5 => registers.b
    case 6 => registers.c
    case _ => throw Exception("Invalid combo operand")
}

object Instr {
  def apply(opcode: Int, operand: Int): Instr = opcode match
    case 0 => ADV(operand)
    case 1 => BXL(operand)
    case 2 => BST(operand)
    case 3 => JNZ(operand)
    case 4 => BXC(operand)
    case 5 => OUT(operand)
    case 6 => BDV(operand)
    case 7 => CDV(operand)
    case _ => throw Exception("No instruction for given opcode")
}

case class ADV(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers.copy(a = (registers.a / Math.pow(2, this.combo(registers)).toLong)), out, ptr + 1)

case class BDV(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers.copy(b = (registers.a / Math.pow(2, this.combo(registers)).toLong)), out, ptr + 1)

case class CDV(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers.copy(c = registers.a / Math.pow(2, this.combo(registers)).toLong), out, ptr + 1)

case class BXL(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers.copy(b = registers.b ^ operand), out, ptr + 1)

case class BST(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers.copy(b = this.combo(registers) % 8), out, ptr + 1)

case class JNZ(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers, out, if (registers.a == 0) ptr + 1 else operand)

case class BXC(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers.copy(b = registers.b ^ registers.c), out, ptr + 1)

case class OUT(operand: Int) extends Instr:
  override def compute(registers: Registers, out: Seq[Int], ptr: Int): (Registers, Seq[Int], Int) =
    (registers, (this.combo(registers) % 8).toInt +: out, ptr + 1)


object day17 extends Day[(Registers, Seq[Int]), String, Long](Year24, 17) {
  private val sample = """Register A: 729
                         |Register B: 0
                         |Register C: 0
                         |
                         |Program: 0,1,5,4,3,0""".stripMargin

  private val sample2 = """Register A: 117440
                          |Register B: 0
                          |Register C: 0
                          |
                          |Program: 0,3,5,4,3,0""".stripMargin

  private val registerRegex = "Register \\w: (\\d+)".r
  private val programRegex = "Program: ([\\d,]+)".r

  def parseInput(input: String): (Registers, Seq[Int]) = {
    val Seq(regInput, programInput) = input.split("\n\n").toSeq
    val Seq(a, b, c) = registerRegex.findAllMatchIn(regInput).map(_.group(1).toLong).toSeq
    val program = programRegex.findFirstMatchIn(programInput).get.group(1).split(",").map(_.toInt).toSeq

    (Registers(a, b, c), program)
  }

  private def parseProgram(program: Seq[Int]): Seq[Instr] =
    program.grouped(2).map { case Seq(opcode, operand) => Instr(opcode, operand) }.toSeq

  @tailrec
  private def compute(registers: Registers, instructions: Seq[Instr], out: Seq[Int] = Seq(), ptr: Int = 0): Seq[Int] = {
    if (ptr >= instructions.size) return out.reverse

    val (updatedRegisters, updatedOut, updatedPtr) = instructions(ptr).compute(registers, out, ptr)
    compute(updatedRegisters, instructions, updatedOut, updatedPtr)
  }

  override def partOne(input: (Registers, Seq[Int])): String = input match
    case (registers: Registers, program: Seq[Int]) => compute(registers, parseProgram(program)).mkString(",")

  override def partTwo(input: (Registers, Seq[Int])): Long = input match
    case (registers: Registers, program: Seq[Int]) => {
      val instructions = parseProgram(program)
      val starts = PQ[Long](0)(Ordering.Long.reverse)

      while (starts.nonEmpty) {
        val lowest = starts.dequeue()

        // The program divides A by 8 each loop. Given this is integer division we know the previous value is from 8 times up to the next multiple
        (lowest * 8 until (lowest + 1) * 8).foreach(start =>
          val result = compute(registers.copy(a = start), instructions)

          if (result == program) return start

          if (program.takeRight(result.size) == result) {
            starts += start
          }
        )
      }

      throw Exception("No identity start found")
    }
}
