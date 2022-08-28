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

  val pat = "[a-zA-Z]+|(_)?[0-9]+|[^a-zA-Z0-9 ().]+|[().]+".r

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
      case l => {
        var listToReturn = List[AObject]()
        var tempList = List[Double]()
        l.foreach(s => {
          val x = strToAObj(s)
          x match {
            case ANumber(n) => tempList = tempList :+ n
            case _ => listToReturn = {
              if(tempList.length > 1) {
                  val y = AVector(tempList.toArray); 
                  tempList = List[Double](); 
                  listToReturn :+ y :+ x
                }
              else if (tempList.length ==  1) {
                  val y = ANumber(tempList.head); 
                  tempList = List[Double]();
                  listToReturn :+ y :+ x
                }
              else {listToReturn :+ x}
            }
          }
        })
        if(tempList.length > 1) {
          listToReturn = listToReturn :+ AVector(tempList.toArray)
        }
        else if(tempList.length == 1) {
          listToReturn = listToReturn :+ ANumber(tempList.head)
        }
        Some(listToReturn)
      }
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
      case (Some(s), Some(num)) if (varName == s) => num
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
              evalWithRightArg(eval1(lineObjs(1)), lineObjs.tail.tail)
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
    case 0 => rightArg match {
                              case ASymbol(s) => getValue(s)
                              case _ => rightArg
                            }
    case 1 => lineObjs.head match {
                              case AOperator(op) => applyMonadicOperation(op, rightArg)
                              case ASymbol(s) => getValue(s)
                              case LRBrac => rightArg
                              case _ => err("Error occurred")
                            } 
    case _ => (lineObjs(0), lineObjs(1)) match {
                          case (AOperator(op), RRBrac) => evalWithRightArg(applyDyadicOperation(op, rightArg, eval(lineObjs.tail.takeWhile(_ != LRBrac))), lineObjs.dropWhile(_ != LRBrac).filter(_ != LRBrac))
                          case (AOperator(op), ASymbol(s)) => evalWithRightArg(applyDyadicOperation(op, rightArg, getValue(s)), lineObjs.tail.tail)
                          case (AOperator(rdc), AOperator(op)) if(rdc == "/" || rdc == "rdc") => evalWithRightArg(applyReductiveOperation(op, rdc, rightArg), lineObjs.tail.tail)
                          case (AOperator(op), AOperator(".")) => evalWithRightArg(applyInOutProduct(lineObjs(2), lineObjs(3), op, rightArg), (2 until lineObjs.tail.tail.length).map(i => lineObjs.tail.tail(i)).toList)
                          case (Assign, ASymbol(s)) => {setValue(s, rightArg); evalWithRightArg(getValue(s), lineObjs.tail.tail);}
                          case (AOperator(op), AOperator(_)) => evalWithRightArg(applyMonadicOperation(op, rightArg), lineObjs.tail)
                          case (AOperator(op), _) => evalWithRightArg(applyDyadicOperation(op, rightArg, lineObjs.tail.head), lineObjs.tail.tail)
                          case _ => err("Error case _ => (lineObjs.head, lineObjs.tail.head) occurred")
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
  // Contains:
  // Functions that describe the monadic, dyadic and reductive version of each operation
  // Functions that deal with applying a monadic, dyadic or reductive operation for a given combination of Scalars, Vectors and/or Matrices
  //
  ///////////////////////////////////////////////////////////////////

  def applyDyadicOperation(op:String, x:AObject, y:AObject): AObject =
    op match {
        case "+" => dyadicOP(dyadicPlus, x, y)
        case "-" => dyadicOP(dyadicMinus, x, y)
        case "div" => dyadicOP(dyadicDiv, x, y)
        case "mul" => dyadicOP(dyadicMul, x, y)
        case "|" => dyadicOP(dyadicMod, x, y)
        case "flr" => dyadicOP(dyadicFlr, x, y)
        case "ceil" => dyadicOP(dyadicCeil, x, y)
        case "!=" => dyadicOP(conditionalTest, x, y)
        case "/" => dyadicSelect(x, y)
        case "mem" => dyadicMem(x, y)
        case "rho" => dyadicRho(x, y)
        case "take" => dyadicTake(x, y)
        case "drop" => dyadicDrop(x, y)
        case _ => err("Error in applyDyadicOperation occurred. op is: " + op + " | x is: " + x + " | y is: " + y)
    }
  
  def applyMonadicOperation(op:String, x:AObject):AObject =
    op match {
        case "+" => monadicOp(monadicPlus, x)
        case "-" => monadicOp(monadicMinus, x)
        case "div" => monadicOp(monadicDiv, x)
        case "mul" => monadicOp(monadicMul, x)
        case "|" => monadicOp(monadicMod, x)
        case "flr" => monadicOp(monadicFlr, x)
        case "ceil" => monadicOp(monadicCeil, x)
        case "~" => monadicOp(logicalNot, x)
        case "iota" => monadicIota(x)
        case "rho" => monadicRho(x)
        case _ => err("Error in applyMonadicOperation occurred")
    }
  
  def applyReductiveOperation(op:String, rdc:String, x:AObject):AObject =
    op match {
        case "+" => reductiveOP(reductivePlus, rdc, x)
        case "-" => reductiveOP(reductiveMinus, rdc, x)
        case "div" => reductiveOP(reductiveDiv, rdc, x)
        case "mul" => reductiveOP(reductiveMul, rdc, x)
        case "|" => reductiveOP(reductiveMod, rdc, x)
        case "flr" => reductiveOP(reductiveFlr, rdc, x)
        case "ceil" => reductiveOP(reductiveCeil, rdc, x)
        case "~" => monadicOp(logicalNot, x)
    }

  def applyInOutProduct(g:AObject, arg:AObject, f:String, a:AObject):AObject = {
    val leftArg = eval1(arg)
    (g, f) match {
      case (AOperator("+"), "mul") => innerProductOp(dyadicMul, reductivePlus, leftArg, a)
      case (AOperator("mul"), "+") => innerProductOp(dyadicPlus, reductiveMul, leftArg, a)
      case (AOperator("flr"), "+") => innerProductOp(dyadicPlus, reductiveFlr, leftArg, a)
      case (AOperator("flr"), "mul") => innerProductOp(dyadicMul, reductiveFlr, leftArg, a)
      case (AOperator("ceil"), "mul") => innerProductOp(dyadicMul, reductiveCeil, leftArg, a)
      case (AOperator("mul"), "flr") => innerProductOp(dyadicFlr, reductiveMul, leftArg, a)
      case (AOperator("flr"), "ceil") => innerProductOp(dyadicCeil, reductiveFlr, leftArg, a)
      case (AOperator("ceil"), "flr") => innerProductOp(dyadicFlr, reductiveCeil, leftArg, a)
      case (AOperator("out"), "+") => outerProductOp(dyadicPlus, leftArg, a)
      case (AOperator("out"), "mul") => outerProductOp(dyadicMul, leftArg, a)
      case (AOperator("out"), "-") => outerProductOp(dyadicMinus, leftArg, a)
      case (AOperator("out"), "flr") => outerProductOp(dyadicFlr, leftArg, a)
      case (AOperator("out"), "ceil") => outerProductOp(dyadicCeil, leftArg, a)
    }
  }
   
  def monadicOp(operation:Double => Double, a:AObject):AObject = 
    a match {
      case ANumber(n) => ANumber(operation(n))
      case AVector(v) if(v.length == 1) => ANumber(operation(v.head))
      case AVector(v) => AVector(v.map(x => operation(x)).toArray)
      case AMatrix(m) => AMatrix(m.map(v => v.map(x => operation(x)).toArray).toArray)
      case _ => err("Error in monadicOp occurred")
    }

  def dyadicOP(operation:(Double, Double) => Double, a:AObject, b:AObject):AObject = 
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
      case _ => err("Error in dyadicOP occurred. a is: :" + a + " | b is: " + b)
    }

  def reductiveOP(operation:Array[Double] => Double, rdc:String, a:AObject):AObject =
    (rdc, a) match {
      case ("/", AVector(v)) => ANumber(operation(v))
      case ("/", AMatrix(m)) => AVector(m.map(v => operation(v)).toArray)
      case ("rdc", AVector(v)) => ANumber(operation(v))
      case ("rdc", AMatrix(m)) => {
        val newM = (0 until m.length).map(i => (0 until m(i).length).map(j => m(j)(i)).toArray).toArray
        AVector(newM.map(v => operation(v)).toArray)
      }
    }

  def innerProductOp(g:(Double, Double) => Double, f:Array[Double] => Double, a:AObject, b:AObject):AObject =
    (a, b) match {
      case (AVector(v1), AVector(v2)) => AVector(v1.map(a => f(v2.map(b => g(a, b)).toArray)).toArray)
      case (AVector(v), AMatrix(m)) => {
        val newM = (0 until m.length).map(i => (0 until m(i).length).map(j => m(j)(i)).toArray).toArray;
        AVector(newM.map(vm => f((0 until vm.length).map(i => g(vm(i), v(i))).toArray)).toArray)
      }
      case (AMatrix(m1), AMatrix(m2)) => AMatrix((0 until m1.length).map(k => (0 until m1.length).map(i => f((0 until m1(i).length).map(j => g(m1(k)(j), m2(j)(i))).toArray)).toArray).toArray)
    }
  
  def outerProductOp(g:(Double, Double) => Double, a:AObject, b:AObject):AObject = 
    (a, b) match {
      case (AVector(v1), AVector(v2)) => AMatrix((0 until v1.length).map(i => (0 until v2.length).map(j => g(v1(i), v2(j))).toArray).toArray)
    }

  def monadicPlus(a:Double):Double = 0.0 + a
  def dyadicPlus(a:Double, b:Double):Double = a + b
  def reductivePlus(a:Array[Double]):Double = a.reduce(_ + _)

  def monadicMinus(a:Double):Double = 0.0 - a
  def dyadicMinus(a:Double, b:Double):Double = a - b
  def reductiveMinus(a:Array[Double]):Double = a.reduce(_ - _)

  def monadicDiv(a:Double):Double = 1.0 / a
  def dyadicDiv(a:Double, b:Double):Double = a / b
  def reductiveDiv(a:Array[Double]):Double = a.reduce(_ / _)

  def monadicMul(a:Double):Double = if(a > 0) 1 else if(a < 0) -1 else 0
  def dyadicMul(a:Double, b:Double):Double = a * b
  def reductiveMul(a:Array[Double]):Double = a.reduce(_ * _)

  def monadicMod(a:Double):Double = if(a > 0) a else if(a < 0) -a else a
  def dyadicMod(a:Double, b:Double):Double = a % b
  def reductiveMod(a:Array[Double]):Double = a.reduce(_ % _)

  def monadicFlr(a:Double):Double = a.floor
  def dyadicFlr(a:Double, b:Double):Double = a min b
  def reductiveFlr(a:Array[Double]):Double = a.reduce(_ min _)

  def monadicCeil(a:Double):Double = a.ceil
  def dyadicCeil(a:Double, b:Double):Double = a max b
  def reductiveCeil(a:Array[Double]):Double = a.reduce(_ max _)
 
  def logicalNot(n:Double):Double = if(n > 0) 0 else if(n <= 0) 1 else 0

  def conditionalTest(a:Double, b:Double):Double = if(a != b) 1 else 0

  def monadicIota(a:AObject):AVector = a match {
    case ANumber(n) => AVector((1.0 to n by 1.0).toArray)
    case AVector(v) if(v.length == 1) => AVector((1.0 to v.head by 1.0).toArray)
  }

  def monadicRho(a:AObject):AVector = 
    a match {
      case ANumber(_) => AVector(Array(0.0))
      case AVector(v) => AVector(Array(v.length))
      case AMatrix(m) => AVector(Array(m.length, m.head.length))
    }

  def dyadicMem(a:AObject, b:AObject):AObject = 
    (a, b) match {
      case (ANumber(n), AVector(v)) => if (v.contains(n)) ANumber(1) else ANumber(0)
      case (AVector(v), ANumber(n)) => if (v.contains(n)) ANumber(1) else ANumber(0)
      case (AMatrix(m), ANumber(n)) => ANumber((memHelper(n, m)))
      case (AMatrix(m), AVector(v)) => AVector(v.map(n => memHelper(n, m)).toArray)
    }
  
  def memHelper(n:Double, m:Array[Array[Double]]):Double = {
    for(i <- 0 until m.length) {
      for(j <- 0 until m(i).length) {
        if(m(i)(j) == n)
          return 1.0
      }
    }
    return 0.0
  }
  
  def dyadicTake(a:AObject, b:AObject):AObject = 
    (a, b) match {
      case (AVector(v), ANumber(n)) => if (n > 0) AVector(v.take(getInt(n))) else AVector(v.takeRight(-(getInt(n))))
      case _ => err("in dyadicTake. a is: " + a + " | b is: " + b)
    }

  def dyadicDrop(a:AObject, b:AObject):AObject = 
    (a, b) match {
      case (AVector(v), ANumber(n)) => if (n > 0) AVector(v.drop(getInt(n))) else AVector(v.dropRight(-(getInt(n))))
      case _ => err("in dyadicDrop")
    }

  def dyadicRho(a:AObject, b:AObject):AObject = 
    (a, b) match {
      case (ANumber(x), ANumber(y)) => AVector((0 until getInt(y)).map(n => x).toArray)
      case (ANumber(n), AVector(v)) => AMatrix((0 until getInt(v(0))).map(x => (0 until getInt(v(1))).map(y => n).toArray).toArray) 
      case (AVector(v), ANumber(n)) => AVector((0 until getInt(n)).map(x => v(x % v.length)).toArray)
      case (AVector(v1), AVector(v2)) => AMatrix((0 until getInt(v2(0))).map(x => ((getInt(v2(1)) * x) until (getInt(v2(1)) * (x + 1))).map(y => v1(y % v1.length)).toArray).toArray)
    }

  def dyadicSelect(a:AObject, b:AObject):AObject = 
    (a, b) match {
      case (AVector(v1), AVector(v2)) => {
          var tempList = List[Double]()
          for(i <- 0 until v2.length) {
            if(v2(i) == 1) tempList = tempList :+ v1(i)
          }
          AVector(tempList.toArray)
        }
    }
}
