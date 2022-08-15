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
      println(tokens)
      val lineObjs = tokensToAObjs(tokens)
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
