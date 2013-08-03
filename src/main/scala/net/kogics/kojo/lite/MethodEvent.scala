/*
 * Copyright (C) 2013 "Sami Jaber" <jabersami@gmail.com>
 * Copyright (C) 2013 Lalit Pant <pant.lalit@gmail.com>
 *
 * The contents of this file are subject to the GNU General Public License
 * Version 3 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.gnu.org/copyleft/gpl.html
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 */
package net.kogics.kojo.lite

import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame

class MethodEvent {
  var ended = false
  var entry: String = _
  var exit: String = _
  var parent: Option[MethodEvent] = None
  var subcalls = Vector[MethodEvent]()
  var entryVars = Vector[(LocalVariable, String)]()
  var returnVal: String = _
  var entryLineNum: Int = _
  var exitLineNum: Int = _
  var methodName: String = _
  var sourceName: String = _
  var callerSourceName: String = _
  var callerLineNum: Int = -1
  var callerLine: String = _
  //  var allVars = Vector[(LocalVariable, String, String)]()
  //  var dclrdArgs = Vector[(LocalVariable, String)]()

  override def toString() = {
    var entryNum = entryLineNum
    var exitNum = exitLineNum
    if (callerLineNum != -1) {
      entryNum = callerLineNum
      exitNum = callerLineNum
    }
    s"""MethodEvent(
    Entry: $entry
    Exit: $exit
    Args: ${entryVars map { vs => val lv = vs._1; s"${lv.name}: ${lv.typeName} = ${vs._2}" }}
    Return value: $returnVal
    Entry Line Number: $entryNum
	Exit Line Number: $exitNum
	Source: $sourceName
	ended: $ended
	)"""
  }
  def isOver() { ended = true }

  def setParent(p: Option[MethodEvent]) {
    parent = p
    parent foreach { _.addChild(this) }
  }

  private def addChild(c: MethodEvent) { subcalls = subcalls :+ c }

  def setEntryVars(stkfrm: StackFrame, localVars: List[LocalVariable]) {
    localVars.foreach { x =>
      val xVal = try {
        stkfrm.getValue(x).toString
      }
      catch {
        case e: Throwable => "unavailable"
      }
      entryVars = entryVars :+ (x, xVal)
    }
  }

  //  def setExitVars(stkfrm: StackFrame, localVars: List[LocalVariable]) {
  //    localVars.foreach { x =>
  //      val xVal = findVal(entryVars, x)
  //      val xVal2 = try {
  //        stkfrm.getValue(x).toString
  //      }
  //      catch {
  //        case e: Throwable => "unavailable"
  //      }
  //      allVars = allVars :+ (x, xVal, xVal2)
  //    }
  //  }
  //
  //  def setArgs(stkfrm: StackFrame, localArgs: List[LocalVariable]) {
  //    localArgs.foreach { x =>
  //      val xVal = try {
  //        stkfrm.getValue(x).toString
  //      }
  //      catch {
  //        case e: Throwable => "unavailable"
  //      }
  //      dclrdArgs = dclrdArgs :+ (x, xVal)
  //    }
  //  }

  def findVal(ls: Vector[(LocalVariable, String)], x: LocalVariable): String = {
    ls.head match {
      case (x, a) => a
      case _      => findVal(ls.tail, x)
    }
  }

  def level: Int = parent match {
    case None    => 0
    case Some(p) => p.level + 1
  }
}
