/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2022 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.mq.interpret

import scala.io.StdIn.readLine

/**
  * The top level object of the application.
  */
object Main {

  import Interpret._

  /**
    * Main entry point of the application.
    *
    * @param args the array of options and parameters passed on 
    * the command line.
    */
  def main(args: Array[String]) {
    while(true)
    {
      println("Input ?   (empty line to terminate)")
      val line = readLine()   // no prompt is given
      if(line.size == 0) return;
      val tokens = matchPat(line)
      val d = List(LRBrac, ANumber(3.0), AOperator("mul"), ANumber(4.0),
                          RRBrac, AOperator("+"), ANumber(5.0))
      val lineObjs = Some(d)//tokensToAObjs(tokens)

      var a = AMatrix(Array(Array(2.0, 3.0), Array(4.0, 1.0)))
      val c = AMatrix(Array(Array(1.0, 5.0), Array(2.0, 2.0)))




      lineObjs match
      {
      case Some(in) => print("input: ")
                       showList(in)
                       println(in)
                       print("result: ")
                       val res = exec(in)
                       showValue(res)
                       println()
                       println(res)
      case _        => println("error")
      }
    }
  }
}
