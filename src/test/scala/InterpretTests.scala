/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2022 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.interpret

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TokensTests extends FlatSpec with Matchers {

  import Interpret._

  def same(a:AObject, b:AObject):Boolean = (a,b) match
  {
  case (Assign, Assign) | (LRBrac, LRBrac) | (RRBrac, RRBrac) => true
  case (ASymbol(f),ASymbol(g)) => f == g
  case (AOperator(f),AOperator(g)) => f == g
  case (ANumber(d),ANumber(e)) => areEqual(d, e)
  case (AVector(u),AVector(v)) if u.size == v.size =>
                                    u.zip(v) forall (t => areEqual(t._1, t._2))
  case (AMatrix(m),AMatrix(n)) if m.size == n.size && m(0).size == n(0).size =>
                                    same(AVector(m.flatten), AVector(n.flatten))
  case _   => false
  }

  def sameTokensToAObjs(s:List[String], r:Option[List[AObject]]):Boolean =
    (tokensToAObjs(s), r) match
    {
    case (None, None)          => true
    case (None, _) | (_, None) => false
    case (Some(a), Some(b)) if a.size == b.size
                               => (a zip b) forall (z => same(z._1, z._2))
    case _                     => false  // different length lists
    }

  def isError(a:AObject):Boolean = a match
  {
  case ASymbol(s) if s startsWith "ERROR" => true
  case _                                  => false
  }

  "PARSING: pattern match" should "handle a number" in {
    assert(matchPat("27") == List("27"))
  }

  it should "handle negative number" in {
    assert(matchPat("_27") == List("_27"))
  }

  it should "handle function symbols" in {
    assert(matchPat("214 + 58 - 32") == List("214", "+", "58", "-", "32"))
  }

  it should "handle names" in {
    assert(matchPat("x mul y flr z") == List("x", "mul", "y", "flr", "z"))
  }

  it should "handle composite symbols" in {
    assert(matchPat("3 != x <- 4") == List("3", "!=", "x", "<-", "4"))
  }

  "strToAObj" should "handle a name" in {
    assert(strToAObj("joe") == ASymbol("joe"))
  }

  it should "handle a number" in {
    assert(strToAObj("1914") == ANumber(1914.0))
  }

  it should "handle a negative number" in {
    assert(strToAObj("_53") == ANumber(-53.0))
  }

  it should "handle an operator" in {
    assert(strToAObj("+") == AOperator("+"))
  }

  "tokensToAObjs" should "handle a single name" in {
    assert(tokensToAObjs(List("abc")) == Some(List(ASymbol("abc"))))
  }

  it should "handle a single number" in {
    assert(tokensToAObjs(List("42")) == Some(List(ANumber(42.0))))
  }

  it should "handle a simple expression" in {
    assert(tokensToAObjs(List("x", "+", "y", "<-", "z")) ==
            Some(List(ASymbol("x"), AOperator("+"), ASymbol("y"),
                                   Assign, ASymbol("z"))))
  }

  it should "handle a more-complex expression" in {
    assert(tokensToAObjs(List("a", "-", "(", "z", "+", "1", ")", "|", "7")) ==
            Some(List(ASymbol("a"), AOperator("-"), LRBrac, ASymbol("z"),
                      AOperator("+"), ANumber(1.0), RRBrac, AOperator("|"),
                                   ANumber(7.0))))
  }

  "SIMPLE EVAL:" should "add two numbers" in {
    assert(same(exec(List(ANumber(4.0), AOperator("+"), ANumber(3.0))),
                ANumber(7.0)))
  }

  it should "mod two numbers" in {
    assert(same(exec(List(ANumber(5.0), AOperator("|"), ANumber(23.0))),
                ANumber(3.0)))
  }

  it should "do iota number" in {
    assert(same(exec(List(AOperator("iota"), ANumber(3.0))),
                AVector(Array(1.0, 2.0, 3.0))))
  }

  it should "do membership" in {
    assert(same(exec(List(ANumber(3.0), AOperator("mem"),
                          AOperator("iota"), ANumber(3.0))),
                ANumber(1.0)))
  }

  it should "do monadic rho" in {
    assert(same(exec(List(AOperator("rho"), AOperator("iota"), ANumber(3.0))),
                AVector(Array(3.0))))
  }

  it should "do dyadic rho" in {
    assert(same(exec(List(AVector(Array(2.0, 2.0)), AOperator("rho"),
                          AOperator("iota"), ANumber(3.0))),
                AMatrix(Array(Array(1.0, 2.0), Array(3.0, 1.0)))))
  }

  it should "do take" in {
    assert(same(exec(List(ANumber(2.0), AOperator("take"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)), 
                          )),
                AVector(Array(3.0, 1.0))))
  }

  "MIXED SHAPE:" should "add number and vector" in {
    assert(same(exec(List(ANumber(4.0), AOperator("+"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)))), 
                AVector(Array(7.0, 5.0, 8.0, 5.0, 9.0))))
  }

  it should "treat one-element vector as scalar" in {
    assert(same(exec(List(AVector(Array(2.0, 3.0)), AOperator("+"),
                          AOperator("iota"), ANumber(1.0))),
                AVector(Array(3.0, 4.0))))
  }

  it should "max vector and vector" in {
    assert(same(exec(List(AVector(Array(7.0, 1.0, 3.0, 2.0, 6.0)), 
                          AOperator("ceil"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)))), 
                AVector(Array(7.0, 1.0, 4.0, 2.0, 6.0))))
  }

  "VARIABLES:" should "work" in {
    assert(same(exec(List(ASymbol("x"), AOperator("+"), ANumber(3.0),
                        AOperator("mul"), ASymbol("x"), Assign, ANumber(7.0))),
                ANumber(28.0)))
  }

  "OPERATORS:" should "reduce a vector with +" in {
    assert(same(exec(List(AOperator("+"), AOperator("/"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)))), 
                ANumber(14.0)))
  }

  it should "reduce a matrix with mul" in {
    assert(same(exec(List(AOperator("mul"), AOperator("/"),
                          AMatrix(Array(Array(2.0, 3.0, 2.0, 7.0, 1.0), 
                                        Array(3.0, 1.0, 4.0, 1.0, 5.0))))), 
                AVector(Array(84.0, 60.0))))
  }

  it should "do outer product" in {
    assert(same(exec(List(AVector(Array(2.0, 3.0, 1.0)), 
                          AOperator("out"), AOperator("."), AOperator("+"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)))), 
                AMatrix(Array(Array(5.0, 3.0, 6.0, 3.0, 7.0), 
                              Array(6.0, 4.0, 7.0, 4.0, 8.0), 
                              Array(4.0, 2.0, 5.0, 2.0, 6.0)))))
  }

  it should "do inner product" in {
    assert(same(exec(List(AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), 
                          AOperator("+"), AOperator("."), AOperator("mul"),
                          AMatrix(Array(Array(1.0, 5.0), Array(2.0, 2.0))))), 
                AMatrix(Array(Array(8.0, 16.0), Array(6.0, 22.0)))))
  }

  "FULL EVAL:" should "handle bracketted expressions" in {
    assert(same(exec(List(LRBrac, ANumber(3.0), AOperator("mul"), ANumber(4.0),
                          RRBrac, AOperator("+"), ANumber(5.0))),
                ANumber(17.0)))
  }

  it should "handle complex expressions" in {
    assert(same(exec(List(AVector(Array(5.0, 1.0, 2.0)), AOperator("+"),
                          LRBrac, ANumber(4.0), AOperator("|"), ANumber(5.0),
                          RRBrac, AOperator("take"), ANumber(2.0),
                          AOperator("drop"), AOperator("iota"), ANumber(5.0))),
                AVector(Array(8.0, 4.0, 5.0))))
  }

  it should "handle complex expressions - primes" in {
    assert(same(exec(List(LRBrac, AOperator("~"), ASymbol("r"),
                          AOperator("mem"), ASymbol("r"), AOperator("out"),
                          AOperator("."), AOperator("mul"), ASymbol("r"),
                          RRBrac, AOperator("/"), ASymbol("r"), Assign,
                          ANumber(1.0), AOperator("drop"), AOperator("iota"),
                          ANumber(20))),
                AVector(Array(2.0, 3.0, 5.0, 7.0, 11.0, 13.0, 17.0, 19.0))))
  }

  ///////////////////////////////////////////////////////////////////
  //
  // TO DO     put your test cases below
  //
  ///////////////////////////////////////////////////////////////////

  "CUSTOM TEST CASES" should "return Assign for <-" in {
    assert(strToAObj("<-") == Assign)
  }

  it should "return LRBrac for (" in {
    assert(strToAObj("(") == LRBrac)
  }

  it should "return just the number even if there's a +" in {
    assert(strToAObj("+34") == ANumber(34.0))
  }

  it should "handle a string operators like mul" in {
    assert(strToAObj("mul") == AOperator("mul"))
  }

  it should "interpret operations without spaces (mul)" in {
    assert(matchPat("2mul3") == List("2", "mul", "3"))
  }

  it should "interpret operations without spaces (+)" in {
    assert(matchPat("2+3") == List("2", "+", "3"))
  }

  it should "interpret operations without spaces (Assign)" in {
    assert(matchPat("2<-3") == List("2", "<-", "3"))
  }

  "MONADIC FUNCTION TEST CASES" should "handle monadic operations (+) for ANumber" in {
    assert(same(exec(List(AOperator("+"), ANumber(4.0))),
                ANumber(4.0)))
  }

  it should "handle monadic operations (+) for AVector" in {
    assert(same(exec(List(AOperator("+"), AVector(Array(4.0, -3.0, 1.0)))),
                AVector(Array(4.0, -3.0, 1.0))))
  }

  it should "handle monadic operations (-) for ANumber" in {
    assert(same(exec(List(AOperator("-"), ANumber(4.0))),
                ANumber(-4.0)))
  }

  it should "handle monadic operations (-) for AVector" in {
    assert(same(exec(List(AOperator("-"), AVector(Array(4.0, -3.0, 1.0)))),
                AVector(Array(-4.0, 3.0, -1.0))))
  }

  it should "handle monadic operations (-) for AMatrix" in {
    assert(same(exec(List(AOperator("-"), AMatrix(Array(Array(4.0, 3.0, 1.0), Array(1.0, 100.5, 232.2), Array(-92.3, -100.0, 5.0))))),
                AMatrix(Array(Array(-4.0, -3.0, -1.0), Array(-1.0, -100.5, -232.2), Array(92.3, 100.0, -5.0)))))
  }

  it should "floor a number" in {
    assert(same(exec(List(AOperator("flr"), ANumber(2.3))), ANumber(2.0)))
  }

  it should "ceil a number" in {
    assert(same(exec(List(AOperator("ceil"), ANumber(2.3))), ANumber(3.0)))
  }
  
  it should "return abs value" in {
    assert(same(exec(List(AOperator("|"), ANumber(-2.3))), ANumber(2.3)))
  }

  "DYADIC FUNCTION TEST CASES" should "handle summing two matricies" in {
    assert(same(exec(List(AMatrix(Array(Array(1.0, 2.0, 3.0), Array(1.0, 2.0, 3.0), Array(1.0, 2.0, 3.0))), AOperator("+"), 
    AMatrix(Array(Array(1.0, 2.0, 3.0), Array(1.0, 2.0, 3.0), Array(1.0, 2.0, 3.0))))),
                AMatrix(Array(Array(2.0, 4.0, 6.0), Array(2.0, 4.0, 6.0), Array(2.0, 4.0, 6.0)))))
  }

    it should "Number rho number" in {
    assert(same(exec(List(ANumber(4.0), AOperator("rho"), ANumber(7.0))),
                AVector(Array(7.0, 7.0, 7.0, 7.0))))
  }

  it should "Number rho Vector" in {
    assert(same(exec(List(ANumber(4.0), AOperator("rho"), AVector(Array(7.0, 8.0, 3.0)))),
                AVector(Array(7.0, 8.0, 3.0, 7.0))))
  }

  it should "Vector rho Number" in {
    assert(same(exec(List(AVector(Array(2.0, 2.0)), AOperator("rho"), ANumber(7.0))),
                AMatrix(Array(Array(7.0, 7.0), Array(7.0, 7.0)))))
  }

  it should "Vector rho Vector" in {
    assert(same(exec(List(AVector(Array(2.0, 3.0)), AOperator("rho"), AVector(Array(4.0, 7.0, 1.0, 9.0)))),
                AMatrix(Array(Array(4.0, 7.0, 1.0), Array(9.0, 4.0, 7.0)))))
  }

  it should "rho Vector rho Vector" in {
    assert(same(exec(List(AOperator("rho"), AVector(Array(2.0, 3.0)), AOperator("rho"), AVector(Array(4.0, 7.0, 1.0, 9.0)))),
                AVector(Array(2.0, 3.0))))
  }

  "OPERATORS:" should "reduce a vector with -" in {
    assert(same(exec(List(AOperator("-"), AOperator("/"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)))), 
                ANumber(-8.0)))
  }

  it should "reduce a vector with div" in {
    assert(same(exec(List(AOperator("div"), AOperator("/"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)))), 
                ANumber(0.15)))
  }

  it should "reduce a matrix with -" in {
    assert(same(exec(List(AOperator("-"), AOperator("/"),
                          AMatrix(Array(Array(2.0, 3.0), Array(4.0, 2.0))))), 
                AVector(Array(-1.0, 2.0))))
  }

  it should "reduce a matrix with div" in {
    assert(same(exec(List(AOperator("div"), AOperator("/"),
                          AMatrix(Array(Array(2.0, 3.0), Array(4.0, 2.0))))), 
                AVector(Array(2.0/3.0, 4.0/2.0))))
  }

  it should "do take with negative number" in {
    assert(same(exec(List(ANumber(-2.0), AOperator("take"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)), 
                          )),
                AVector(Array(1.0, 5.0))))
  }

  it should "do drop" in {
    assert(same(exec(List(ANumber(2.0), AOperator("drop"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)), 
                          )),
                AVector(Array(4.0, 1.0, 5.0))))
  }

  it should "do drop with negative number" in {
    assert(same(exec(List(ANumber(-2.0), AOperator("drop"),
                          AVector(Array(3.0, 1.0, 4.0, 1.0, 5.0)), 
                          )),
                AVector(Array(3.0, 1.0, 4.0))))
  }

  it should "find minimum" in {
    assert(same(exec(List(ANumber(500.0), AOperator("flr"), ANumber(2.3))), ANumber(2.3)))
  }

  it should "find maximum" in {
    assert(same(exec(List(ANumber(500.0), AOperator("ceil"), ANumber(2.3))), ANumber(500.0)))
  }


  "INNER PRODUCT:" should "calculate inner product (mul then +) then add to matrix" in {
    assert(same(exec(List(AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), AOperator("+"), AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), 
                          AOperator("+"), AOperator("."), AOperator("mul"),
                          AMatrix(Array(Array(1.0, 5.0), Array(2.0, 2.0))))), 
                AMatrix(Array(Array(10.0, 19.0), Array(10.0, 23.0)))))
  }

  it should "calculate inner product (+ then mul) then add to matrix" in {
    assert(same(exec(List(AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), AOperator("+"), AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), 
                          AOperator("mul"), AOperator("."), AOperator("+"),
                          AMatrix(Array(Array(1.0, 5.0), Array(2.0, 2.0))))), 
                AMatrix(Array(Array(17.0, 38.0), Array(19.0, 28.0)))))
  }

  it should "calculate inner product (ceil then flr) then add to matrix" in {
    assert(same(exec(List(AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), AOperator("+"), AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0))), 
                          AOperator("ceil"), AOperator("."), AOperator("flr"),
                          AMatrix(Array(Array(1.0, 5.0), Array(2.0, 2.0))))), 
                AMatrix(Array(Array(4.0, 5.0), Array(5.0, 5.0)))))
  }

  it should "calculate inner product (mul then +) of a vector and matrix" in {
    assert(same(exec(List(AVector(Array(1.0, 2.0)), 
                          AOperator("+"), AOperator("."), AOperator("mul"),
                          AMatrix(Array(Array(1.0, 2.0), Array(3.0, 4.0))))), 
                AVector(Array(7.0, 10.0))))
  }

  it should "calculate inner product (flr then mul) of a vector and matrix" in {
    assert(same(exec(List(AVector(Array(1.0, 2.0)), 
                          AOperator("mul"), AOperator("."), AOperator("flr"),
                          AMatrix(Array(Array(1.0, 2.0), Array(3.0, 4.0))))), 
                AVector(Array(2.0, 2.0))))
  }

  "OUTER PRODUCT:" should "calculate outer product of two vectors (mul)" in {
    assert(same(exec(List(AVector(Array(1.0, 2.0, 3.0, 4.0)), AOperator("out"), AOperator("."), AOperator("mul"), AVector(Array(5.0, 10.0, 15.0, 20.0)))), 
                AMatrix(Array(Array(5.0, 10.0, 15.0, 20.0), Array(10.0, 20.0, 30.0, 40.0), 
                              Array(15.0, 30.0, 45.0, 60.0), Array(20.0, 40.0, 60.0, 80.0)))))
  }

  it should "calculate outer product of two vectors (flr)" in {
    assert(same(exec(List(AVector(Array(1.0, 2.0, 3.0, 4.0)), AOperator("out"), AOperator("."), AOperator("flr"), AVector(Array(5.0, 10.0, 15.0, 20.0)))), 
                AMatrix(Array(Array(1.0, 1.0, 1.0, 1.0), Array(2.0, 2.0, 2.0, 2.0), 
                              Array(3.0, 3.0, 3.0, 3.0), Array(4.0, 4.0, 4.0, 4.0)))))
  }

  "REDUCTIVE OPERATOR:" should "reduce columns of matrix with rdc" in {
    assert(same(exec(List(AOperator("+"), AOperator("rdc"), AVector(Array(2.0, 2.0)), AOperator("rho"), AVector(Array(1.0, 2.0, 3.0, 4.0)))), 
                AVector(Array(4.0, 6.0))))
  }

  it should "reduce columns of matrix with rdc then operate on result" in {
    assert(same(exec(List(AVector(Array(2.0, 2.0)),AOperator("mul"),AOperator("+"), AOperator("rdc"), AVector(Array(2.0, 2.0)), 
                          AOperator("rho"), AVector(Array(1.0, 2.0, 3.0, 4.0)))), 
                AVector(Array(8.0, 12.0))))
  }


  "BRACKETS:" should "do multiple brackets" in {
    assert(same(exec(List(ANumber(5), AOperator("+"), LRBrac, ANumber(10.3), AOperator("flr"), ANumber(2), RRBrac, AOperator("+"),
                          LRBrac, AOperator("iota"), ASymbol("x"), Assign, ANumber(4.0), RRBrac, AOperator("+"),
                          AVector(Array(1.0, 2.0, 3.0, 4.0)))), 
                AVector(Array(9.0, 11.0, 13.0, 15.0))))
  }

  it should "do nested brackets" in {
    assert(same(exec(List(LRBrac, LRBrac, ASymbol("x"), Assign, ANumber(3.0), RRBrac, AOperator("+"), ANumber(1.0), RRBrac, AOperator("rho"), ANumber(7.0))), 
                AVector(Array(7.0, 7.0, 7.0, 7.0))))
  }
}
