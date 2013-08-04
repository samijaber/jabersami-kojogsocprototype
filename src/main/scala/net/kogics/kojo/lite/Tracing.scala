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

import java.awt.Color
import java.awt.geom.Point2D
import java.io.File
import java.lang.reflect.InvocationTargetException

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.internal.util.Position
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.Reporter
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable
import scala.util.matching.Regex

import com.sun.jdi.AbsentInformationException
import com.sun.jdi.Bootstrap
import com.sun.jdi.IntegerValue
import com.sun.jdi.LocalVariable
import com.sun.jdi.ObjectReference
import com.sun.jdi.StackFrame
import com.sun.jdi.StringReference
import com.sun.jdi.ThreadReference
import com.sun.jdi.VirtualMachine
import com.sun.jdi.connect.Connector
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.EventSet
import com.sun.jdi.event.MethodEntryEvent
import com.sun.jdi.event.MethodExitEvent
import com.sun.jdi.event.ThreadStartEvent
import com.sun.jdi.event.VMDeathEvent
import com.sun.jdi.event.VMDisconnectEvent
import com.sun.jdi.event.VMStartEvent
import com.sun.jdi.request.EventRequest

import net.kogics.kojo.core.Turtle
import net.kogics.kojo.util.Utils

class Tracing(scriptEditor: ScriptEditor, builtins: Builtins) {
  var evtSet: EventSet = _
  var currThread: ThreadReference = _
  var mainThread: ThreadReference = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()
  var turtles = Vector[Turtle]()
  var turtlesRefs = Vector[Long]()
  var runningInBG = false
  val imgPath = "main/resources"

  var currEvtVec = Vector[(String, Option[MethodEvent])](("main", None))
  var CurrMthdEvtIndex: Int = -1

  val reporter = new Reporter {
    override def info0(position: Position, msg: String, severity: Severity, force: Boolean) {
      severity.count += 1
      println(msg)
    }
  }
  val compiler = new Global(settings, reporter)
  val tracingGUI = new TracingGUI(scriptEditor, builtins.kojoCtx)
  val lineNumOffset = 3

  val wrapperCode = """object Wrapper {
import net.kogics.kojo.lite.TracingBuiltins._
def main(args: Array[String]) { 
    %s
  }
} 
"""

  def stop() {
    currThread.virtualMachine().exit(1)
  }

  def compile(code0: String) = {
    val code = wrapperCode format code0
    val codeFile = new BatchSourceFile("scripteditor", code)
    val run = new compiler.Run
    reporter.reset
    run.compileSources(List(codeFile))
    if (reporter.hasErrors) {
      throw new RuntimeException("Trace Compilation Error. Ensure that your program compiles correctly before trying to trace it.")
    }
  }

  def makeSettings() = {
    val iSettings = new Settings()
    iSettings.usejavacp.value = true
    iSettings.outputDirs.setSingleOutput(tmpdir)
    iSettings
  }

  def launchVM() = {
    var connector: LaunchingConnector = null
    val conns = Bootstrap.virtualMachineManager().allConnectors();
    breakable {
      for (conn <- conns) {
        if (conn.name().equals("com.sun.jdi.CommandLineLaunch")) {
          connector = conn.asInstanceOf[LaunchingConnector]
          break
        }
      }
    }

    // set connector arguments
    val connArgs = connector.defaultArguments();
    val mArgs = connArgs.get("main").asInstanceOf[Connector.Argument]
    val opts = connArgs.get("options").asInstanceOf[Connector.Argument]
    var optionValue = s"""-classpath "$tmpdir${File.pathSeparator}${System.getProperty("java.class.path")}" """

    if (mArgs == null)
      throw new Error("Bad launching connector");

    mArgs.setValue("Wrapper"); // assign args to main field
    opts.setValue(optionValue) //assign args to options field

    val vm = connector.launch(connArgs)
    vm
  }

  val ignoreMethods = Set("main", "<init>", "<clinit>", "$init$", "repeat", "repeatWhile", "runInBackground")
  val turtleMethods = Set("setBackground", "color", "forward", "right", "left", "turn", "clear", "cleari", "invisible", "jumpTo", "back", "setPenColor", "setFillColor", "setAnimationDelay", "setPenThickness", "penDown", "penUp", "circle", "savePosHe", "restorePosHe", "newTurtle", "changePosition", "scaleCostume", "setCostumes", "axesOn", "axesOff", "gridOn", "gridOff", "zoom")

  def getThread(vm: VirtualMachine, name: String): ThreadReference = {
    try {
      //Find main thread in target VM
      val allThrds = vm.allThreads
      var index: Int = allThrds.indexWhere(x => x.name == name)
      allThrds(index)
    }
    catch {
      case e: ArrayIndexOutOfBoundsException =>
        println("a Thread with name " + name + "does not exist")
        return null
    }
  }

  def trace(code: String) = Utils.runAsync {
    try {
      turtles = Vector[Turtle]()
      turtlesRefs = Vector[Long]()
      currEvtVec = Vector[(String, Option[MethodEvent])](("main", None))

      compile(code)
      //Connect to target VM
      val vm = launchVM()
      println("Attached to process '" + vm.name + "'")

      //Create Event Requests
      val excludes = Array("java.*", "javax.*", "sun.*", "com.sun.*", "com.apple.*")
      mainThread = getThread(vm, "main")
      currThread = getThread(vm, "main")
      createRequests(excludes, vm, mainThread);
      WatchThreadStarts

      //Iterate through Events
      val evtQueue = vm.eventQueue
      vm.resume

      tracingGUI.reset

      breakable {
        while (true) {
          evtSet = evtQueue.remove()
          for (evt <- evtSet.eventIterator) {
            evt match {
              case threadStartEvt: ThreadStartEvent =>
                var name = threadStartEvt.thread().name
                if (name.contains("Thread-")) {
                  createRequests(excludes, vm, threadStartEvt.thread())
                  incrementCurrEvt(name)
                }
                println("Thread started : " + name)
              case methodEnterEvt: MethodEntryEvent =>
                currThread = methodEnterEvt.thread()
                if (currThread.name.contains("AWT-")) {
                  println("thread is " + currThread.name + " with method entered: " + methodEnterEvt.method().name)
                }

                if (!(ignoreMethods.contains(methodEnterEvt.method.name) || methodEnterEvt.method.name.startsWith("apply"))) {
                  try {
                    val frame = methodEnterEvt.thread().frame(0)
                    val toprint = try {
                      if (methodEnterEvt.method().arguments().size > 0)
                        "(%s)" format methodEnterEvt.method.arguments.map { n =>
                          val frameVal = frame.getValue(n)

                          val argval = if (false) {
                            val objRef = frameVal.asInstanceOf[ObjectReference]
                            val mthd = objRef.referenceType.methodsByName("toString")(0)
                            val rtrndValue = objRef.invokeMethod(currThread, mthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
                            rtrndValue.asInstanceOf[StringReference].value()
                          }
                          else {
                            frameVal
                          }
                          s"arg ${n.name}: ${n.typeName} = $argval"
                        }.mkString(",")
                      else "()"
                    }
                    catch {
                      case e: AbsentInformationException => ""
                    }

                    val desc = s"[Method Enter] ${methodEnterEvt.method.name}$toprint"
                    var argList = List[LocalVariable]()
                    try { argList = methodEnterEvt.method.arguments.toList }
                    catch { case e: AbsentInformationException => }

                    val callerSrcName = try { currThread.frame(1).location.sourceName }
                    catch { case _: Throwable => "N/A" }

                    val callerLineNum = try { currThread.frame(1).location().lineNumber - lineNumOffset }
                    catch { case _: Throwable => -1 }

                    val callerLine = try { scriptEditor.getTraceLine(callerLineNum) }
                    catch { case _: Throwable => "N/A" }

                    val srcName = try { methodEnterEvt.location().sourceName }
                    catch { case e: Throwable => "" }

                    handleMethodEntry(
                      methodEnterEvt.method.name,
                      desc,
                      turtleMethods contains methodEnterEvt.method.name,
                      currThread.frame(0),
                      argList,
                      methodEnterEvt.location.lineNumber - lineNumOffset,
                      srcName,
                      callerSrcName,
                      callerLine,
                      callerLineNum
                    )
                  }
                  catch {
                    case inv: InvocationTargetException => println(inv.printStackTrace())
                    case t: Throwable =>
                      println(t.printStackTrace())
                    //println(s"[Exception] [Method Enter] [${t.getClass()}] ${methodEnterEvt.method.name} -- ${t.getMessage}")
                  }
                }

              case methodExitEvt: MethodExitEvent =>
                if (!(ignoreMethods.contains(methodExitEvt.method.name) || methodExitEvt.method.name.startsWith("apply"))) {
                  try {
                    currThread = methodExitEvt.thread()
                    //Vectors to keep track of new turtlesRefs
                    if (methodExitEvt.method.name == "newTurtle" && isFromWrapper(currThread.frame(0))) {
                      var ref = methodExitEvt.returnValue().asInstanceOf[ObjectReference].uniqueID()
                      println("new turtle taken into account with ref: " + ref.toString)
                      turtlesRefs = turtlesRefs :+ ref
                    }
                    //determine if the method is a Turtle API method
                    if (turtleMethods contains methodExitEvt.method.name) {
                      val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(
                        desc,
                        true,
                        currThread.frame(0),
                        currThread.frame(1).location.lineNumber - lineNumOffset,
                        methodExitEvt.returnValue.toString
                      )
                    }
                    else {
                      val desc = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      def rtrnVal: String = { if (methodExitEvt.returnValue() == null) "" else methodExitEvt.returnValue.toString }

                      handleMethodExit(
                        desc,
                        false,
                        currThread.frame(0),
                        methodExitEvt.location.lineNumber - lineNumOffset,
                        rtrnVal
                      )
                    }
                  }
                  catch {
                    case t: Throwable =>
                      println(t.printStackTrace())
                    //println(s"[Exception] [Method Exit] [${t.getClass()}] ${methodExitEvt.method.name} -- ${t.getMessage}")
                  }
                }
              case vmDcEvt: VMDisconnectEvent =>
                println("VM Disconnected"); break
              case vmStartEvt: VMStartEvent =>
                println("VM Started")
              case vmDeathEvt: VMDeathEvent =>
                println("VM Dead")
              case _ =>
                println("Other")
            }
          }
          evtSet.resume()
        }
      }
    }
    catch {
      case t: Throwable => System.err.println(s"[Exception] -- ${t.getMessage}")
      //t.printStackTrace()
    }
  }

  def printFrameVarInfo(stkfrm: StackFrame) {
    try {
      println(s"Visible Vars: ${stkfrm.visibleVariables}")
      println(s"Argument Values: ${stkfrm.getArgumentValues}")
    }
    catch {
      case t: Throwable =>
    }
  }

  def getCurrMthdEvtIndex: Int = try {
    var name = currThread.name
    currEvtVec.indexWhere(evt => evt._1 == name)
  }
  catch {
    case t: Throwable => println("could not find MethodEventIndex for thread " + currThread.name); -1
  }

  def getCurrentMethodEvent: Option[MethodEvent] = try {
    var name = currThread.name
    var index = currEvtVec.indexWhere(evt => evt._1 == name)
    currEvtVec(index)._2
  }
  catch {
    case t: Throwable =>
      println("could not find MethodEvent for thread " + currThread.name + ". New entry created.")
      /*
       * Temporary solution? Create a new index for any thread with no previous entry in our Vector. 
       */
      incrementCurrEvt(currThread.name)
      currEvtVec.last._2
  }

  def updateMethodEventVector(newEvt: Option[MethodEvent]) {
    var index = currEvtVec.indexWhere(evt => evt._1 == currThread.name)
    currEvtVec = currEvtVec.updated(index, (currThread.name, newEvt))
  }

  def incrementCurrEvt(name: String) {
    if (currEvtVec.indexWhere(x => x._1 == name) == -1)
      currEvtVec = currEvtVec :+ (name, None)
  }

  def handleMethodEntry(name: String, desc: String, isTurtle: Boolean, stkfrm: StackFrame, localArgs: List[LocalVariable], lineNum: Int, source: String, callerSource: String, callerLine: String, callerLineNum: Int) {
    var newEvt = new MethodEvent()
    var mthdEvent = getCurrentMethodEvent
    newEvt.entry = desc
    newEvt.entryLineNum = lineNum
    newEvt.setEntryVars(stkfrm, localArgs)
    newEvt.setParent(mthdEvent)
    newEvt.sourceName = source
    newEvt.callerSourceName = callerSource
    newEvt.callerLine = callerLine
    newEvt.callerLineNum = callerLineNum
    newEvt.methodName = name

    updateMethodEventVector(Some(newEvt))

    var ret: Option[(Point2D.Double, Point2D.Double)] = None
    if (isTurtle) {
      ret = runTurtleMethod(name, stkfrm, localArgs)
    }
    tracingGUI.addEvent(newEvt, ret)
  }

  def handleMethodExit(desc: String, isTurtle: Boolean, stkfrm: StackFrame, lineNum: Int, retVal: String) {
    var mthdEvent = getCurrentMethodEvent
    mthdEvent.foreach { ce =>
      ce.isOver()
      ce.exit = desc
      ce.exitLineNum = lineNum
      ce.returnVal = retVal

      tracingGUI.addEvent(ce, None)

      mthdEvent = ce.parent
    }
    updateMethodEventVector(mthdEvent)
  }

  def isFromWrapper(stkfrm: StackFrame): Boolean = { stkfrm.thisObject().toString().contains("TracingBuiltins") }

  def runTurtleMethod(name: String, stkfrm: StackFrame, localArgs: List[LocalVariable]): Option[(Point2D.Double, Point2D.Double)] = {
    var ret: Option[(Point2D.Double, Point2D.Double)] = None
    var frame = if (stkfrm.thisObject() == null) {
      currThread.frame(1)
    }
    else {
      stkfrm
    }

    var stdTurtle = if (name == "newTurtle") true
    else {
      isFromWrapper(frame)
    }

    import builtins.Tw
    import builtins.TSCanvas
    var turtle: Turtle = Tw.getTurtle
    if (!stdTurtle) {
      var caller = frame.thisObject().uniqueID()
      var index = turtlesRefs.indexOf(caller)
      println("caller ID is " + caller + 
          ", ref found is " + turtlesRefs(index) +
          ", function being called is " + name +
          ", All refs are: ")
      turtlesRefs.foreach(println(_))
      if (index != -1)
        turtle = turtles(index)
    }

    name match {
      /*
      case "right" | "left"=>
        val method = Tw.getClass().getMethod(name)
        if (localArgs.length == 0) {
          method.invoke(Tw.getClass().newInstance())
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          method.invoke(Tw.getClass().newInstance(), angle: java.lang.Double)
        }
        */
      case "clear" =>
        TSCanvas.clear()
      case "cleari" =>
        TSCanvas.cleari()
      case "invisible" =>
        turtle.invisible
      case "forward" =>
        val step = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.forward(step)
        ret = Some(turtle.lastLine)
      case "right" =>
        if (!stdTurtle) {
          //do nothing
        }
        else if (stdTurtle && localArgs.length == 0) {
          turtle.right()
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          turtle.right(angle)
        }
      case "left" =>
        if (!stdTurtle) {
          //do nothing
        }
        else if (stdTurtle && localArgs.length == 0) {
          turtle.left()
        }
        else {
          val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
          turtle.left(angle)
        }
      case "turn" =>
        val angle = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.turn(angle)
      case "back" =>
        val step = stkfrm.getValue(localArgs(0)).toString.toDouble
        if (stdTurtle) {
          turtle.back(step)
        }
      case "home" =>
        turtle.home
      case "jumpTo" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.jumpTo(x, y)
      case "setCostume" =>
        val str = stkfrm.getValue(localArgs(0)).toString
        turtle.setCostume(str)
      case "setPosition" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.setPosition(x, y)
      case "setPenColor" =>
        val color = getColor(stkfrm, localArgs)
        turtle.setPenColor(color)
      case "setFillColor" =>
        val color = getColor(stkfrm, localArgs)
        turtle.setFillColor(color)
      case "setAnimationDelay" =>
        val step = stkfrm.getValue(localArgs(0)).toString.toLong
        turtle.setAnimationDelay(step)
      case "setPenThickness" =>
        val thickness = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.setPenThickness(thickness)
      case "penUp" =>
        turtle.penUp
      case "penDown" =>
        turtle.penDown
      case "circle" =>
        val r = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.circle(r)
      case "savePosHe" =>
        turtle.savePosHe
      case "restorePosHe" =>
        turtle.restorePosHe
      case "newTurtle" =>
        if (localArgs.length == 2) {
          val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
          turtles = turtles :+ TSCanvas.newTurtle(x, y)
        }
        else {
          val (x, y, str) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble, stkfrm.getValue(localArgs(2)).toString)
          turtles = turtles :+ TSCanvas.newTurtle(x, y, str.slice(1, str.length - 1))
        }
      case "changePosition" =>
        val (x, y) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble)
        turtle.changePosition(x, y)
      case "scaleCostume" =>
        val a = stkfrm.getValue(localArgs(0)).toString.toDouble
        turtle.scaleCostume(a)
      case "setCostumes" =>
        val (a, b) = (stkfrm.getValue(localArgs(0)).toString, stkfrm.getValue(localArgs(1)).toString)
        turtle.setCostumes(a, b)
      case "setBackground" =>
        var c = getColor(stkfrm, localArgs)
        TSCanvas.tCanvas.setCanvasBackground(c)
      case "axesOn" =>
        TSCanvas.axesOn
      case "axesOff" =>
        TSCanvas.axesOff
      case "gridOn" =>
        TSCanvas.gridOn
      case "gridOff" =>
        TSCanvas.gridOff
      case "zoom" =>
        val (x, y, z) = (stkfrm.getValue(localArgs(0)).toString.toDouble, stkfrm.getValue(localArgs(1)).toString.toDouble, stkfrm.getValue(localArgs(0)).toString.toDouble)
        TSCanvas.zoom(x, y, z)
      case _ =>
    }
    ret
  }

  def getColor(stkfrm: StackFrame, localArgs: List[LocalVariable]): Color = {
    var colorVal = stkfrm.getValue(localArgs(0)).asInstanceOf[ObjectReference]

    var mthd = colorVal.referenceType.methodsByName("toString")(0)
    var rtrndValue = colorVal.invokeMethod(currThread, mthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
    var str = rtrndValue.asInstanceOf[StringReference].value()
    var pattern = new Regex("\\d{1,3}")
    var rgb = Vector[Int]()
    (pattern findAllIn str).foreach(c => rgb = rgb :+ c.toInt)

    var alphaMthd = colorVal.referenceType.methodsByName("getAlpha")(0)
    var alphaValue = colorVal.invokeMethod(currThread, alphaMthd, new java.util.ArrayList, ObjectReference.INVOKE_SINGLE_THREADED)
    var alpha = alphaValue.asInstanceOf[IntegerValue].value

    new Color(rgb(0), rgb(1), rgb(2), alpha)
  }

  def WatchThreadStarts() {
    val evtReqMgr = currThread.virtualMachine().eventRequestManager

    val thrdStartVal = evtReqMgr.createThreadStartRequest()
    thrdStartVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    //thrdStartVal.addThreadFilter(mainThread)
    thrdStartVal.enable()
  }

  def createRequests(excludes: Array[String], vm: VirtualMachine, thread: ThreadReference) {
    val evtReqMgr = vm.eventRequestManager

    val mthdEnterVal = evtReqMgr.createMethodEntryRequest()
    excludes.foreach { mthdEnterVal.addClassExclusionFilter(_) }
    mthdEnterVal.addThreadFilter(thread)
    mthdEnterVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdEnterVal.enable()

    val mthdExitVal = evtReqMgr.createMethodExitRequest()
    excludes.foreach { mthdExitVal.addClassExclusionFilter(_) }
    mthdExitVal.addThreadFilter(thread)
    mthdExitVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdExitVal.enable()
  }

}