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

  val pat = "(\\+|_)?[a-z0-9+-.*.~=(<=)<>(>=)(!=),|/]+".r

  def matchPat(line:String):List[String] =
                              pat.findAllIn(line.toLowerCase).toList

  def operators = List("mul", "div", "flr", "ceil", "iota", "out",
                       "rho", "mem", "take", "drop",
                       "+", "-", "/", "*", ".", "~", "=", "<=",
                       "<", ">" , ">=", "!=", ",", "|", "rdc")

  def strToAObj(s:String):AObject = {
    val numPat = "(_?)([0-9]+)".r
    val opPat = "([+-.*.~=(<=)<>(>=)(!=),|/]+)".r
    s match {
      case numPat(sign, num) => if (sign == "") ANumber(num.toDouble) else (ANumber(-(num.toDouble)))
      case opPat(op) => if (op == "<-") Assign else if (op == "(" ) LRBrac else if (op == ")") RRBrac else AOperator(op)
      case n => ASymbol(n)
      case _ => err("unknown token")
    }
  }

  def tokensToAObjs(a:List[String]):Option[List[AObject]] =
    a match {
      case l => Some(l map (x => strToAObj(x) match {
        case obj @ ANumber(_) => obj
        case obj @ AOperator(_) => obj
        case obj @ Assign => obj
        case obj @ RRBrac => obj
        case obj @ LRBrac => obj
        case obj @ ASymbol(_) => obj
      }))
      case _ => None
    }


  // for testing
  def lineToAObjs(line:String):List[AObject] =
    tokensToAObjs(matchPat(line)) match
    {
    case Some(s) => s
    case None    => List()
    }

  def setValue(varName:String, value:AObject):Unit =
  {
    // TO DO: assign a value to a variable
  }

  def getValue(varName:String):AObject =
  {
    // TO DO: get the value of a variable; or error if variable not defined
    err("variable " + varName + " undefined")
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
    case 1 => err("TO DO")  // just one thing to the left of rightArg
    case _ => err("TO DO")  // more than one thing to the left of rightArg
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
