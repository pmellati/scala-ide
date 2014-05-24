package org.scalaide.debug.internal.editor

import org.junit.Assert._
import org.junit.Test
import org.scalaide.core.testsetup.TestProjectSetup
import org.scalaide.debug.internal.ScalaDebugTestSession
import org.junit.Before
import org.junit.After
import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.runtime.NullProgressMonitor
import org.scalaide.debug.internal.ScalaDebugRunningTest
import org.scalaide.debug.internal.ScalaDebugger
import org.scalaide.core.internal.jdt.model.ScalaCompilationUnit
import org.scalaide.core.testsetup.SDTTestUtils
import scala.reflect.internal.util.Position
import scala.util.Try

object StackFrameVariableOfTreeFinderTest
extends TestProjectSetup("sfValFinding", bundleName = "org.scala-ide.sdt.debug.tests")
with ScalaDebugRunningTest

class StackFrameVariableOfTreeFinderTest {
  import StackFrameVariableOfTreeFinderTest._

  var session: ScalaDebugTestSession = _

  val ScalaClassName = "valfinding.ScalaClass"
  val OuterClassName = "valfinding.OuterClass"
  val BaseClassName = "valfinding.BaseClass"
  val DerivedClassName = "valfinding.DerivedClass"
  val ExtenderClassName = "valfinding.ExplicitExtenderOfTheTrait"
  val TheTraitName = "valfinding.TheTrait"
  val EnclosingTraitName = "valfinding.EnclosingTrait"
  val ObjectName = "valfinding.Objectt"
  val ClosureTestClassName = "valfinding.ClosureTest"


  @Before
  def refreshBinaryFiles() {
    project.underlying.build(IncrementalProjectBuilder.CLEAN_BUILD, new NullProgressMonitor)
    project.underlying.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, new NullProgressMonitor)
  }

  @After
  def cleanDebugSession() {
    if (session ne null) {
      session.terminate()
      session = null
    }
  }

  @Test
  def tests() {
    val cu = compilationUnit("ValFindingDemo.scala").asInstanceOf[ScalaCompilationUnit]
    cu.withSourceFile {(src, compiler) =>
      import compiler.{Try => _, _}

      val treeCache = collection.mutable.Map.empty[String, Tree]

      def treeAtMarker(markerName: String): Tree = treeCache.getOrElseUpdate(markerName, {
        val positions = SDTTestUtils.positionsOf(src.content, s" /*{$markerName}*/")
        assertEquals(s"Couldn't find exactly one occurence of marker $markerName in ${src.path}.", 1, positions.size)
        val markerPos = Position.offset(src, positions.head - 1)

        val resp = new Response[Tree]
        askTypeAt(markerPos, resp)
        resp.get.left getOrElse (throw new RuntimeException(s"Failed to get the Tree at marker $markerName."))
      })

      def assertFoundValue(atMarker: String, when: String = null, is: Option[String]) {
        val whenClause = if(when!=null) s"($when) " else ""
        val foundVariable = StackFrameVariableOfTreeFinder.find(src, compiler, ScalaDebugger.currentStackFrame)(treeAtMarker(atMarker))
        if(is.isDefined)
          foundVariable map {v =>
            val valueStr = Try{v.getValue.getValueString}.getOrElse {
              throw new AssertionError(s"Failed to '.getValue()' on the found variable at marker '$atMarker'.")}
            val expected = is.get
            assertTrue(
                s"Stack-frame value found at marker '$atMarker' $whenClause was expected to be '$expected'," +
                s" but is actually $valueStr.",
                valueStr.matches(s""""$expected"""" + """ \(id=\d+\)"""))
          } getOrElse fail(s"Failed to find stack-frame value at marker '$atMarker'.")
        else if(foundVariable.isDefined)
          fail(s"Expected to find no value at marker '$atMarker' $whenClause, " +
               s"however the value ${foundVariable.get.getValue.getValueString} was found.")
      }

      session = ScalaDebugTestSession(file("ValFindingDemo.launch"))


      session.runToLine(ScalaClassName, 32)

      assertFoundValue(atMarker="class param & field decl",  is=Some("fieldClassParam"))
      assertFoundValue(atMarker="class param & field usage", is=Some("fieldClassParam"))
      assertFoundValue(atMarker="class field decl",  is=Some("nonFieldClassParamfieldClassParam"))
      assertFoundValue(atMarker="class field usage", is=Some("nonFieldClassParamfieldClassParam"))
      assertFoundValue(atMarker="field with same name of a field", is=None)
      assertFoundValue(atMarker="similarly named field decl of a class we are not in",  is=None)
      assertFoundValue(atMarker="similarly named field usage of a class we are not in", is=None)

      session.runToLine(ScalaClassName, 36)

      assertFoundValue(atMarker="method param decl",  is=Some("func param"))
      assertFoundValue(atMarker="method param usage", is=Some("func param"))
      assertFoundValue(atMarker="similarly named param of a method we are not in", is=None)
      assertFoundValue(atMarker="method-local variable", "when the variable is not yet assigned", is=None)

      session.stepOver

      assertFoundValue(atMarker="method-local variable", "after the variable is assigned", is=Some("local val"))
      assertFoundValue(atMarker="similarly named local var of a method we are not in", is=None)

      session.runToLine(ScalaClassName, 46)

      assertFoundValue(atMarker="nested method param", is=Some("nesteds parameter"))
      assertFoundValue(atMarker="nested method local", is=Some("nested2Local"))
      assertFoundValue(atMarker="enclosing nested method local", is=Some("nested1Local"))
      assertFoundValue(atMarker="root enclosing method local", is=Some("local val"))

      session.runToLine(OuterClassName, 71)

      assertFoundValue(atMarker="inner class field decl", is=Some("Inner's Field"))

      session.runToLine(OuterClassName, 77)

      assertFoundValue(atMarker="method-local var shadowing field", is=Some("Local Shadower"))
      assertFoundValue(atMarker="shadowed field accessed with this", is=Some("Inner's Field"))
      assertFoundValue(atMarker="shadowed field accessed with this with class name", is=Some("Inner's Field"))
      assertFoundValue(atMarker="shadowed field accessed with this with enclosing class name", is=Some("Outer's Field"))
      assertFoundValue(atMarker="exclusive field of enclosing class", is=Some("outerExclusiveField"))

      session.runToLine(BaseClassName, 90)

      assertFoundValue(atMarker="base class field decl", is=Some("baseField"))
      assertFoundValue(atMarker="base class field usage", is=Some("baseField"))
      assertFoundValue(atMarker="base class method param", is=Some("base meth param"))

      session.runToLine(DerivedClassName, 101)

      assertFoundValue(atMarker="base class field usage from derived class", is=Some("baseField"))
      assertFoundValue(atMarker="derived class field usage", is=Some("derived field"))

      session.runToLine(ExtenderClassName, 115)

      assertFoundValue(atMarker="trait field access from ctor of extender", is=Some("traitFieldd"))

      session.runToLine(ExtenderClassName, 118)

      assertFoundValue(atMarker="trait field access from method of extender", is=Some("traitFieldd"))

      session.runToLine(TheTraitName, 110)

      assertFoundValue(atMarker="trait method param", is=Some("traitFuncParam"))
      assertFoundValue(atMarker="trait field decl", is=Some("traitFieldd"))
      assertFoundValue(atMarker="trait field usage from trait", is=Some("traitFieldd"))

      session.runToLine(EnclosingTraitName, 129)

      assertFoundValue(atMarker="field decl of class nested in trait", is=Some("nField"))
      assertFoundValue(atMarker="field usage of class nested in trait", is=Some("nField"))
      assertFoundValue(atMarker="field of enclosing trait usage", is=Some("tField"))

      session.runToLine(ObjectName, 139)

      assertFoundValue(atMarker="object field decl",  is=Some("obj field"))
      assertFoundValue(atMarker="object field usage", is=Some("obj field"))

      session.runToLine(ClosureTestClassName, 151)

      assertFoundValue(atMarker="closure param decl", is=Some("clParam1"))
      assertFoundValue(atMarker="closure param usage", is=Some("clParam1"))
      assertFoundValue(atMarker="captured field of enclosing class", is=Some("captured field"))
      assertFoundValue(atMarker="captured local variable of enclosing method", is=Some("Local val captured"))
      assertFoundValue(atMarker="local of another method named similarly to a local of closure", is=None)

      session.runToLine(ClosureTestClassName, 155)

      assertFoundValue(
          atMarker="local var of closure shadowing local var of enclosing method",
          "before the variable is assigned to", is=None)

      session.stepOver

      assertFoundValue(
          atMarker="local var of closure shadowing local var of enclosing method",
          "after the variable is assigned to", is=Some("shadowed in closure"))
    }
  }
}