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
  case (ANumber(d),ANumber(e)) => areEqual(d, e)
  case (AVector(u),AVector(v)) if u.size == v.size =>
                                    u.zip(v) forall (t => areEqual(t._1, t._2))
  case (AMatrix(m),AMatrix(n)) if m.size == n.size && m(0).size == n(0).size =>
                                    same(AVector(m.flatten), AVector(n.flatten))
  case _   => false
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
}
