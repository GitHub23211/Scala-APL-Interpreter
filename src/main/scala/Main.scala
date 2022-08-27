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
      val d = List(LRBrac, AOperator("~"), ASymbol("r"),
                          AOperator("mem"), ASymbol("r"), AOperator("out"),
                          AOperator("."), AOperator("mul"), ASymbol("r"),
                          RRBrac, AOperator("/"), ASymbol("r"), Assign,
                          ANumber(1.0), AOperator("drop"), AOperator("iota"),
                          ANumber(20))
      val lineObjs = Some(d)//tokensToAObjs(tokens)

      var a = Array(1, 0, 1, 1)
      val c = Array(10, 20, 30, 40)
      val b = (0 until a.length).map(i => if(a(i) == 1) c(i)).toList
      println(b)




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
