package net.kogics.kojo.lite

import java.io.File

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import com.sun.jdi.Bootstrap
import com.sun.jdi.LocalVariable
import com.sun.jdi.StackFrame
import com.sun.jdi.ThreadReference
import com.sun.jdi.VirtualMachine
import com.sun.jdi.connect.Connector
import com.sun.jdi.connect.LaunchingConnector
import com.sun.jdi.event.EventSet
import com.sun.jdi.event.MethodEntryEvent
import com.sun.jdi.event.MethodExitEvent
import com.sun.jdi.event.VMDisconnectEvent
import com.sun.jdi.request.EventRequest

import net.kogics.kojo.util.Utils

class Tracing(scriptEditor: ScriptEditor) {

  var evtSet: EventSet = _
  var initconn: LaunchingConnector = _
  var mainThread: ThreadReference = _
  var codeFile: BatchSourceFile = _
  val tmpdir = System.getProperty("java.io.tmpdir")
  val settings = makeSettings()
  val compiler = new Global(settings)
  val tracingGUI = new TracingGUI(scriptEditor)

  val wrapperCode = """object Wrapper { 
  def main(args: Array[String]) { 
    %s
  }
    
  def clear() {}
  def forward(n: Double) {}
  def right(n: Double) {}
  def repeat(n: Int) (fn: => Unit) {
    var i = 0
    while(i < n) {
      fn
      i += 1
    }
  }
} 
"""

  def compile(code0: String) = {
    val code = wrapperCode format code0
    codeFile = new BatchSourceFile("scripteditor", code)
    val run = new compiler.Run
    run.compileSources(List(codeFile))

  }

  def makeSettings() = {
    val iSettings = new Settings()
    iSettings.usejavacp.value = true
    iSettings.outputDirs.setSingleOutput(tmpdir)
    iSettings
  }

  def getVM(initconn: LaunchingConnector) = {
    var connector = initconn

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
    var optionValue = s"-classpath $tmpdir${File.pathSeparator}/tmp/scala-library.jar"

    if (mArgs == null)
      throw new Error("Bad launching connector");

    mArgs.setValue("Wrapper"); // assign args to main field
    opts.setValue(optionValue) //assign args to options field

    val vm = connector.launch(connArgs)
    vm
  }

  val ignoreMethods = Set("main", "<init>", "<clinit>")
  def trace(code: String) = Utils.runAsync {

    val result = compile(code)
    //Connect to target VM
    val vm = getVM(initconn)
    println("Attached to process '" + vm.name + "'")

    //Create Event Requests
    val excludes = Array("java.*", "javax.*", "sun.*", "com.sun.*", "com.apple.*")
    createRequests(excludes, vm);

    //Iterate through Events
    val evtQueue = vm.eventQueue
    vm.resume

    //Find main thread in target VM
    val allThrds = vm.allThreads
    allThrds.foreach { x => if (x.name == "main") mainThread = x }

    tracingGUI.reset

    breakable {
      while (true) {
        evtSet = evtQueue.remove()
        for (evt <- evtSet.eventIterator) {
          evt match {
            case methodEnterEvt: MethodEntryEvent =>
              if (!(ignoreMethods contains methodEnterEvt.method.name)) {
                try {
                  val frame = mainThread.frame(0)
                  val toprint =
                    if (methodEnterEvt.method().arguments().size > 0)
                      "(%s)" format methodEnterEvt.method.arguments.map { n =>
                        val argval = frame.getValue(n)
                        val argname = n.name
                        s"arg ${n.name}: ${n.`type`} = $argval"
                      }.mkString(",")
                    else ""

                  //determine if the method is a Turtle API method
                  methodEnterEvt.method().name match {
                    case "forward" | "right" | "clear" =>
                      var strng = s"[Method Enter] ${methodEnterEvt.method().name}" + toprint
                      handleMethodEntry(strng, true, mainThread.frame(0), methodEnterEvt.method.arguments.toList, mainThread.frame(1).location().lineNumber - 2)
                    case _ =>
                      var strng = s"[Method Enter] ${methodEnterEvt.method().name}" + toprint
                      handleMethodEntry(strng, false, mainThread.frame(0), methodEnterEvt.method.arguments.toList, methodEnterEvt.location.lineNumber - 2)
                  }
                }
                catch {
                  case t: Throwable =>
                    println(s"[Exception] [Method Enter] ${methodEnterEvt.method.name} -- ${t.getMessage}")
                }
              }
            case methodExitEvt: MethodExitEvent =>
              if (!(ignoreMethods contains methodExitEvt.method.name)) {
                try {
                  //determine if the method is a Turtle API method
                  methodExitEvt.method().name match {
                    case "forward" | "right" | "clear" =>
                      var strng = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(strng, true, mainThread.frame(0), mainThread.frame(1).location.lineNumber - 2, methodExitEvt.returnValue.toString)
                    case _ =>
                      var strng = s"[Method Exit] ${methodExitEvt.method().name}(return value): " + methodExitEvt.returnValue
                      handleMethodExit(strng, false, mainThread.frame(0), methodExitEvt.location.lineNumber - 2, methodExitEvt.returnValue.toString)
                  }
                }
                catch {
                  case t: Throwable =>
                    println(s"[Exception] [Method Exit] ${methodExitEvt.method.name} -- ${t.getMessage}")
                }
              }
            case vmDcEvt: VMDisconnectEvent =>
              println("VM Disconnected"); break
            case _ => println("Other")
          }
        }
        evtSet.resume()
      }
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

  var currentMethodEvent: Option[MethodEvent] = None
  def handleMethodEntry(desc: String, isTurtle: Boolean, stkfrm: StackFrame, localArgs: List[LocalVariable], lineNum: Int) {
    var newEvt = new MethodEvent()
    newEvt.entry = desc
    newEvt.entryLineNum = lineNum
    newEvt.setEntryVars(stkfrm, localArgs)
    newEvt.setParent(currentMethodEvent)
    currentMethodEvent = Some(newEvt)
    tracingGUI.addEvent(currentMethodEvent.get)
  }

  def handleMethodExit(desc: String, isTurtle: Boolean, stkfrm: StackFrame, lineNum: Int, retVal: String) {
    currentMethodEvent.foreach { ce =>
      ce.isOver()
      ce.exit = desc
      ce.exitLineNum = lineNum
      ce.returnVal = retVal
      tracingGUI.addEvent(currentMethodEvent.get)
      currentMethodEvent = ce.parent
    }
  }

  def createRequests(excludes: Array[String], vm: VirtualMachine) {
    val evtReqMgr = vm.eventRequestManager

    val mthdEnterVal = evtReqMgr.createMethodEntryRequest()
    excludes.foreach { mthdEnterVal.addClassExclusionFilter(_) }
    mthdEnterVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdEnterVal.enable()

    val mthdExitVal = evtReqMgr.createMethodExitRequest()
    excludes.foreach { mthdExitVal.addClassExclusionFilter(_) }
    mthdExitVal.setSuspendPolicy(EventRequest.SUSPEND_EVENT_THREAD)
    mthdExitVal.enable()
  }

}