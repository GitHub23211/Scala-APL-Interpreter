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
    case AVector(v) => "AVector(" + (v map (_.toString)).reduce(_ + ", " + _) +
                               ")"
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

  def extractNumber(obj:AObject):Double = 
    obj match {
      case ANumber(n) => n
      case _ => 0.0
    }

  def extractVector(obj:AObject):Array[Double] = 
    obj match {
      case AVector(v) => v
      case _ => Array(0.0)
    }
  
  def applyDyadicOperation(op:String, x:AObject, y:AObject): AObject =
    op match {
        case "+" => ANumber(extractNumber(x) + extractNumber(y))
        case "-" => ANumber(extractNumber(x) - extractNumber(y))
        case "div" => ANumber(extractNumber(x) / extractNumber(y))
        case "mul" => ANumber(extractNumber(x) * extractNumber(y))
        case "|" => ANumber(extractNumber(x) % extractNumber(y))
        case "mem" => if (extractVector(y).contains(extractNumber(x))) ANumber(1) else ANumber(0)
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
        case "take" => {
          val vx = extractVector(x)
          extractNumber(y) match {
            case num if(num < 0) => AVector(((vx.length - 1) until (vx.length - 1 - getInt(num)) by -1).map(a => vx(a)).toArray)
            case num if(num > 0) => AVector((0 until getInt(num)).map(a => vx(a)).toArray)
          }
        }
        case _ => err("Error in applyDyadicOperation occurred")
    }
  
  def applyMonadicOperation(op:String, x:AObject): AObject =
    op match {
        case "+" => ANumber(0 + extractNumber(x))
        case "-" => ANumber(0 - extractNumber(x))
        case "div" => ANumber(1 / extractNumber(x))
        case "mul" => extractNumber(x) match {
                                        case n if(n > 0) => ANumber(1)
                                        case n if(n < 0) => ANumber(-1)
                                        case _ => ANumber(0)
                                      }
        case "|" => extractNumber(x) match {
                                      case n if(n < 0) => ANumber(-(n))
                                      case obj @ _ => ANumber(obj)
                                    }
        case "iota" => AVector((1.0 to extractNumber(x) by 1.0).toArray)
        case "rho" => x match {
                              case ANumber(_) => AVector(Array(0.0))
                              case AVector(v) => AVector(Array(v.length))
                              case AMatrix(m) => AVector(Array(m.length, m.head.length))
                            }
        case _ => err("Error in applyMonadicOperation occurred")
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
                              case ANumber(n) => lineObjs.head
                              case _ => err("Error occurred")
                            } 
    case _ => lineObjs.head match {
                          case AOperator(op) => lineObjs.tail.head match {
                                                                case ANumber(_) => evalWithRightArg(applyDyadicOperation(op, rightArg, lineObjs.tail.head), lineObjs.tail.tail)
                                                                case AOperator(_) => evalWithRightArg(applyMonadicOperation(op, rightArg), lineObjs.tail)
                                                                case AVector(_) => evalWithRightArg(applyDyadicOperation(op, rightArg, lineObjs.tail.head), lineObjs.tail.tail)
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

}
