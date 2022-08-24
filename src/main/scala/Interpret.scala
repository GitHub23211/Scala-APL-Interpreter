/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2022 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

package org.mq.interpret

import scala.util.matching._

object Interpret {

  val ran = scala.util.Random

  sealed abstract class AObject
  {
    override def toString:String = this match
    {
    case ASymbol(sym) => "ASymbol(" + sym + ")"
    case AOperator(op) => "AOperator(" + op + ")"
    case ANumber(d) => "ANumber(" + d + ")"
    case AVector(Array()) => ""                               // this is the added line
    case AMatrix(m) => "AMatrix(" + (m map (v =>
                                          (v map (_.toString)).
                                                      reduce(_ + ", " + _))).
                                                    reduce(_ + "; " + _) + ")"
    case LRBrac     => "LRBrac"
    case RRBrac     => "RRBrac"
    case Assign     => "Assign"
    case _          => super.toString
    }
  }
  case class ASymbol(sym:String) extends AObject
  case class AOperator(op:String) extends AObject
  case class ANumber(num:Double) extends AObject
  case class AVector(v:Array[Double]) extends AObject
  case class AMatrix(m:Array[Array[Double]]) extends AObject
  case object LRBrac extends AObject    // "("
  case object RRBrac extends AObject    // ")"
  case object Assign extends AObject    // "<-"

  def err(msg:String):AObject = ASymbol("ERROR " + msg)

  def isError(a:AObject):Boolean = a match
  {
  case ASymbol(s) if s.startsWith("ERR") => true
  case _    => false
  }

  def isNumObj(a:AObject):Boolean = a match
  {
  case ANumber(_) => true
  case AVector(_) => true
  case AMatrix(_) => true
  case _          => false
  }

  def isInt(d:Double):Boolean = (d - getInt(d)).abs < 0.00001

  def getInt(d:Double):Int = (d + (if(d >= 0) 0.001 else -0.001)).toInt

  def isBool(d:Double):Boolean = areEqual(d, 0.0) || areEqual(d, 1.0)

  def areEqual(d:Double, e:Double):Boolean =
            d == 0 && e == 0 || (d - e).abs < 0.0001 * (d.abs + e.abs)

  val pat = "[a-zA-Z]+|(_)?[0-9]+|[^a-zA-Z0-9 ]+".r

  def matchPat(line:String):List[String] =
                              pat.findAllIn(line.toLowerCase).toList

  def operators = List("mul", "div", "flr", "ceil", "iota", "out",
                       "rho", "mem", "take", "drop",
                       "+", "-", "/", "*", ".", "~", "=", "<=",
                       "<", ">" , ">=", "!=", ",", "|", "rdc")

  def strToAObj(s:String):AObject = {
    val char = s.head
    char match {
      case '+' if s.tail != "" => ANumber(s.tail.toDouble)
      case '_' => ANumber(-(s.tail.toDouble))
      case _ if char.isDigit => ANumber(s.toDouble)
      case _ if operators.contains(s) => AOperator(s)
      case '(' => LRBrac
      case ')' => RRBrac
      case '<' if s.tail == "-" => Assign
      case n => ASymbol(s)
      case _ => err("unknown token")
    }
  }

  def tokensToAObjs(a:List[String]):Option[List[AObject]] =
    a match {
      case l => Some(l map (x => strToAObj(x)))
      case _ => None
    }


  // for testing
  def lineToAObjs(line:String):List[AObject] =
    tokensToAObjs(matchPat(line)) match
    {
    case Some(s) => s
    case None    => List()
    }

  var variable:(Option[String], Option[AObject]) = (None, None)

  def setValue(varName:String, value:AObject):Unit =
  {
    // TO DO: assign a value to a variable
    variable = (Some(varName), Some(value))
  }

  def getValue(varName:String):AObject =
  {
    variable match {
      case (Some(n), Some(num)) if (n == varName) => num
      case _ => err("variable " + varName + " undefined")
    }
  }

  // determines whether a indicates the presence of a value
  def isValue(a:AObject):Boolean = a match
  {
  case ASymbol(s) => true    // variable
  case ANumber(n) => true
  case AVector(n) => true
  case AMatrix(n) => true
  case RRBrac     => true    // start of a bracketted expression
  case _          => false
  }

  def isOperator(a:AObject):Boolean = a match
  {
  case AOperator(op) => true
  case _             => false
  }

  // evaluates a list of objects
  def exec(lineObjs:List[AObject]):AObject = eval(lineObjs.reverse)

  // evaluates one object
  def eval1(a:AObject):AObject = a match
  {
  case ASymbol(s) if s.startsWith("ERR") => a  // retain error messages
  case ASymbol(s) => getValue(s)               // get variable's value
  case ANumber(n) => a
  case AVector(n) => a
  case AMatrix(n) => a
  case _          => err("eval1")
  }

  // evaluates a (backwards) list of objects
  def eval(lineObjs:List[AObject]):AObject = lineObjs.size match
  {
  case 0 => err("eval: no objs")
  case 1 => eval1(lineObjs.head)
  case _ => if(lineObjs(0) == RRBrac)
            {
              err("TO DO")   // TO DO: handle bracketted expression
            }
            else
              evalWithRightArg(eval1(lineObjs.head), lineObjs.tail)
  }

  // evaluate the rest of the line now that rightArg has been evaluated
  // line looks like:      ... rightArg
  // where    ...   is lineObjs (backwards)
  // returns the value of the whole line
  def evalWithRightArg(rightArg:AObject, lineObjs:List[AObject]):AObject =
  {
    // if rightArg is an error object then don't try to evaluate the rest
    // of the line; instead, return this error
    if(isError(rightArg)) return rightArg

    lineObjs.size match
    {
    case 0 => rightArg    // end of line
    case 1 => lineObjs.head match {
                              case AOperator(op) => applyMonadicOperation(op, rightArg)
                              case _ => err("Error occurred")
                            } 
    case _ => lineObjs.head match {
                          case AOperator(op) => lineObjs.tail.head match {
                                                                case ANumber(_) => evalWithRightArg(applyDyadicOperation(op, rightArg, lineObjs.tail.head), lineObjs.tail.tail)
                                                                case AOperator(_) => evalWithRightArg(applyMonadicOperation(op, rightArg), lineObjs.tail)
                                                                case AVector(_) => evalWithRightArg(applyDyadicOperation(op, rightArg, lineObjs.tail.head), lineObjs.tail.tail)
                                                                case AMatrix(_) => evalWithRightArg(applyDyadicOperation(op, rightArg, lineObjs.tail.head), lineObjs.tail.tail)
                                                                case _ => err("Error case _ => lineObjs.head match, case AOperator(op) => lineObjs.tail.head match occurred")
                                                              }
                          case ANumber(n) => evalWithRightArg(rightArg, lineObjs.tail)
                          case _ => err("Error evalWithRightArg, case _ => lineObjs.head match, case ANumber(n) occurred")
                        }
    }
  }

  // displays a double, as integer if applicable
  def prtDbl(d:Double):Unit = { if(isInt(d)) print(getInt(d)) else print(d)
                                print(" ") }

  // displays an object
  def showValue(a:AObject):Unit = a match
  {
  case ASymbol(s) => print(s); print(" ")
  case AOperator(op) => print(op); print(" ")
  case ANumber(d) => prtDbl(d)
  case AVector(v) => v foreach prtDbl
  case AMatrix(m) => println
                     for(r <- 0 until m.size)
                     {
                       for(c <- 0 until m(r).size)
                         prtDbl(m(r)(c))
                       println
                     }
  case LRBrac     => print("( ")
  case RRBrac     => print(") ")
  case Assign     => print("<- ")
  case _          => print("ERRORP")
  }

  // displays a list of objects
  def showList(s:List[AObject]):Unit =
  {
    s foreach showValue
    println()
  }

  ///////////////////////////////////////////////////////////////////
  //
  // Functions that deal with applying each operation for a given combination of Scalars, Vectors and/or Matrices
  //
  ///////////////////////////////////////////////////////////////////

    def extractNumber(obj:AObject):Double = 
    obj match {
      case ANumber(n) => n
      case AVector(n) if(n.length == 1) => n.head
      case _ => 0.0
    }

  def extractVector(obj:AObject):Array[Double] = 
    obj match {
      case AVector(v) => v
      case _ => Array(0.0)
    }
  
  def applyDyadicOperation(op:String, x:AObject, y:AObject): AObject =
    op match {
        case "+" => testDyadic(dyadicPlus, x, y)
        case "-" => testDyadic(dyadicMinus, x, y)
        case "div" => testDyadic(dyadicDiv, x, y)
        case "mul" => testDyadic(dyadicMul, x, y)
        case "|" => testDyadic(dyadicMod, x, y)
        case "mem" => if (extractVector(y).contains(extractNumber(x))) ANumber(1) else ANumber(0)
        case "flr" => testDyadic(dyadicFlr, x, y)
        case "ceil" => testDyadic(dyadicCeil, x, y)
        case "rho" => y match {
                              case ANumber(ny) =>  x match {
                                                      case ANumber(nx) => AVector((0 until getInt(ny)).map(x => nx).toArray)
                                                      case AVector(vx) => AVector((0 until getInt(ny)).map(x => vx(x % vx.length)).toArray)
                                                    }
                              case AVector(vy) => x match {
                                                      case ANumber(xn) => AMatrix((0 until getInt(vy(0))).map(x => (0 until getInt(vy(1))).map(y => xn).toArray).toArray)
                                                      case AVector(vx) => AMatrix((0 until getInt(vy(0))).map(x => ((getInt(vy(1)) * x) until (getInt(vy(1)) * (x + 1))).map(y => vx(y % vx.length)).toArray).toArray)
                                                    }
                            }
        case "take" => dyadicTake(x, y)
        case "drop" => dyadicDrop(x, y)
        case _ => err("Error in applyDyadicOperation occurred")
    }
  
  def applyMonadicOperation(op:String, x:AObject): AObject =
    op match {
        case "+" => testMonadic(monadicPlus, x)
        case "-" => testMonadic(monadicMinus, x)
        case "div" => testMonadic(monadicDiv, x)
        case "mul" => testMonadic(monadicMul, x)
        case "|" => testMonadic(monadicMod, x)
        case "iota" => AVector(monadicIota(x))
        case "rho" => x match {
                              case ANumber(_) => AVector(Array(0.0))
                              case AVector(v) => AVector(Array(v.length))
                              case AMatrix(m) => AVector(Array(m.length, m.head.length))
                            }
        case "flr" => testMonadic(monadicFlr, x)
        case "ceil" => testMonadic(monadicCeil, x)
        case "~" => testMonadic(logicalNOT, x)
        case _ => err("Error in applyMonadicOperation occurred")
    }

  def testMonadic(operation:Double => Double, a:AObject): AObject = {
    a match {
      case ANumber(n) => ANumber(operation(n))
      case AVector(v) if(v.length == 1) => ANumber(operation(v.head))
      case AVector(v) => AVector(v.map(x => operation(x)).toArray)
      case AMatrix(m) => AMatrix(m.map(x => x.map(y => operation(y)).toArray).toArray)
      case _ => err("Error in testMonadic occurred")
    }
  }

  def testDyadic(operation:(Double, Double) => Double, a:AObject, b:AObject): AObject = {
    (a, b) match {
      case (ANumber(x), ANumber(y)) => ANumber(operation(x, y))
      case (ANumber(n), AVector(v)) if(v.length == 1) => ANumber(operation(n, v.head))
      case (ANumber(n), AVector(v)) => AVector(v.map(a => operation(a, n)).toArray)
      case (AVector(v), ANumber(n)) => AVector(v.map(a => operation(a, n)).toArray)
      case (ANumber(n), AMatrix(m)) => AMatrix(m.map(v => v.map(a => operation(a, n)).toArray).toArray)
      case (AMatrix(m), ANumber(n)) => AMatrix(m.map(v => v.map(a => operation(a, n)).toArray).toArray)
      case (AVector(v1), AVector(v2)) if(v1.length == 1) => AVector(v2.map(a => operation(a, v1.head)).toArray)
      case (AVector(v1), AVector(v2)) if(v2.length == 1) => AVector(v1.map(a => operation(a, v2.head)).toArray)
      case (AVector(v1), AVector(v2)) if(v1.length == 1 & v2.length == 1) => ANumber(operation(v1.head, v2.head))
      case (AVector(v1), AVector(v2)) if(v1.length == v2.length) => AVector((0 until v1.length).map(i => operation(v1(i), v2(i))).toArray)
      case (AMatrix(m1), AMatrix(m2)) if(m1.length == m2.length & m1.head.length == m2.head.length) => 
            AMatrix((0 until m1.length).map(i => (0 until m1(i).length).map(j => operation(m1(i)(j), m2(i)(j))).toArray).toArray)
      case _ => err("Error in testDyadic occurred")
    }
  }

  def monadicPlus(a:Double):Double = 0.0 + a
  def dyadicPlus(a:Double, b:Double):Double = a + b

  def monadicMinus(a:Double):Double = 0.0 - a
  def dyadicMinus(a:Double, b:Double):Double = a - b

  def monadicDiv(a:Double):Double = 1.0 / a
  def dyadicDiv(a:Double, b:Double):Double = a / b

  def monadicMul(a:Double):Double = if(a > 0) 1 else if(a < 0) -1 else 0
  def dyadicMul(a:Double, b:Double):Double = a * b

  def monadicMod(a:Double):Double = if(a > 0) a else if(a < 0) -a else a
  def dyadicMod(a:Double, b:Double):Double = a % b

  def monadicFlr(a:Double):Double = a.floor
  def dyadicFlr(a:Double, b:Double):Double = a min b

  def monadicCeil(a:Double):Double = a.ceil
  def dyadicCeil(a:Double, b:Double):Double = a max b

  def logicalNOT(a:Double):Double = if(a > 0) 0 else if(a < 0) 1 else 0

  def monadicIota(a:AObject):Array[Double] = a match {
    case ANumber(n) => (1.0 to n by 1.0).toArray
    case AVector(v) if(v.length == 1) => (1.0 to v.head by 1.0).toArray
  }

  def dyadicTake(a:AObject, b:AObject):AVector = {
    (a, b) match {
      case (AVector(v), ANumber(n)) if(n < 0) => AVector(((v.length + getInt(n)) until v.length).map(a => v(a)).toArray)
      case (AVector(v), ANumber(n)) if(n > 0) => AVector((0 until getInt(n)).map(a => v(a)).toArray)
    }
  }

  def dyadicDrop(a:AObject, b:AObject):AVector = {
      (a, b) match {
        case (AVector(v), ANumber(n)) if(n < 0) => AVector((0 until (v.length + getInt(n))).map(a => v(a)).toArray)
        case (AVector(v), ANumber(n)) if(n > 0) => AVector((getInt(n) until v.length).map(a => v(a)).toArray)
      }
    }
}
