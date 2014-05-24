package org.scalaide.debug.internal.editor

import scala.util.Try
import scala.reflect.internal.util.{Position, RangePosition, SourceFile}
import org.scalaide.util.internal.eclipse.EclipseUtils.PimpedRegion
import org.scalaide.ui.internal.editor.ScalaHover
import org.scalaide.ui.editor.extensionpoints.{ TextHoverFactory => TextHoverFactoryInterface }
import org.scalaide.debug.internal.ScalaDebugger
import org.scalaide.debug.internal.model.{ScalaThisVariable, ScalaStackFrame}
import org.scalaide.core.internal.jdt.model.ScalaCompilationUnit
import org.scalaide.core.compiler.ScalaPresentationCompiler
import org.eclipse.swt.widgets.Shell
import org.eclipse.jface.text.ITextViewer
import org.eclipse.jface.text.ITextHoverExtension2
import org.eclipse.jface.text.ITextHoverExtension
import org.eclipse.jface.text.ITextHover
import org.eclipse.jface.text.IRegion
import org.eclipse.jface.text.IInformationControlExtension2
import org.eclipse.jface.text.IInformationControlCreator
import org.eclipse.jface.text.DefaultInformationControl
import org.eclipse.jdt.internal.debug.ui.ExpressionInformationControlCreator
import org.eclipse.debug.core.model.IVariable

class TextHoverFactory extends TextHoverFactoryInterface {
  def createFor(scu: ScalaCompilationUnit): ITextHover = new ScalaHover(scu) with ITextHoverExtension with ITextHoverExtension2 {
    var returnedStringAtGetHoverInfo2 = false

    def getHoverInfo2(viewer: ITextViewer, region: IRegion): AnyRef = {
      icu.withSourceFile({ (src, compiler) =>
        import compiler._

        val resp = new Response[Tree]
        askTypeAt(region.toRangePos(src), resp)

        returnedStringAtGetHoverInfo2 = false

        (for {
          t <- resp.get.left.toOption
          stackFrame <- Option(ScalaDebugger.currentStackFrame)
          variable <- StackFrameVariableOfTreeFinder.find(src, compiler, stackFrame)(t)
        } yield variable) getOrElse {
          returnedStringAtGetHoverInfo2 = true
          super.getHoverInfo(viewer, region)
        }
      }).get
    }

    def getHoverControlCreator: IInformationControlCreator =
      if(returnedStringAtGetHoverInfo2)
        new IInformationControlCreator {
          def createInformationControl(parent: Shell) =
            new StringHandlingInformationControlExtension2(parent)
        }
      else  /* An IVariable was returned. */
        new ExpressionInformationControlCreator
  }

  class StringHandlingInformationControlExtension2(parent: Shell)
  extends DefaultInformationControl(parent)
  with IInformationControlExtension2 {
    def setInput(input: AnyRef) {
      setInformation(input.asInstanceOf[String])
    }
  }
}


object StackFrameVariableOfTreeFinder {
  def find(src: SourceFile, compiler: ScalaPresentationCompiler, stackFrame: ScalaStackFrame)(t: compiler.Tree): Option[IVariable] = {
    import compiler.{Try => _, _}

    // ---------------- HELPERS ---------------------------------
    /////////////////////////////////////////////////////////////

    def treeAt(pos: Position) = {
      val resp = new Response[Tree]
      askTypeAt(pos, resp)
      resp.get.left.toOption
    }

    // StackFrame line numbering is 1-based, while SourceFile line numbering is 0-based. Hence the "- 1".
    lazy val sfLineNumber = Try{stackFrame.getLineNumber}.filter(_ > 0).map(_ - 1).toOption

    lazy val stackFramePos = sfLineNumber.map {ln =>
      Position.offset(src, src.skipWhitespace(src.lineToOffset(ln)))
    }

    def isStackFrameWithin(range: RangePosition) = (for {
      stackFrameLineNum <- sfLineNumber
      stackFramePos = Position.offset(src, src.lineToOffset(stackFrameLineNum))
    } yield range.includes(stackFramePos)) getOrElse false

    /** Here we use 'template' as an umbrella term to refer to any entity that exposes a 'this'
     *  variable in the stack frame. This 'this' variable will contain the fields of this template.
     *  Also if an instance X of a template is enclosed by an instance Y of a template, then Y is
     *  accessible from X, via the 'outer' field of X.
     *
     *  Currently this file considers classes, objects, traits & closures to be templates.
     *
     *  Since the stack frame only reveals its line number (and not its position within the line),
     *  sometimes it is difficult to pinpoint, with certainty, the template that encloses its
     *  position.
     *
     *  This function returns the certainly inner-most template that encloses the line of the stack
     *  frame. None is returned if we cannot be certain.
     */
    def innerMostTemplateCertainlyEnclosingSfPos: Option[Symbol] = {
      sealed trait Result
      case object NoEnclTemplateFound extends Result
      case object Uncertainty extends Result
      case class InnerMostEnclTemplate(templDef: Tree) extends Result

      // Assumes that sfLine is within t.pos.
      def innerMostEnclIn(t: Tree, sfLine: Int): Result = {
        var tIsATemplate = false
        if(t.isInstanceOf[ClassDef] || t.isInstanceOf[ModuleDef] || t.isInstanceOf[Function]) {
          tIsATemplate = true

          val templStartLine = src.offsetToLine(t.pos.start)
          val templEndLine = src.offsetToLine(t.pos.end)
          if(sfLine == templStartLine || sfLine == templEndLine)
            return Uncertainty  // Because in these lines, other statements may exist as well.
        }

        val childrenResults = t.children.filter {child =>
          Try{isStackFrameWithin(child.pos.asInstanceOf[RangePosition])}.getOrElse(false)
        } map {innerMostEnclIn(_, sfLine)}

        if(childrenResults contains Uncertainty)
          Uncertainty
        else childrenResults.find(_.isInstanceOf[InnerMostEnclTemplate]).getOrElse{
          if(tIsATemplate) InnerMostEnclTemplate(t)
          else NoEnclTemplateFound
        }
      }

      val resp = new Response[Tree]
      askLoadedTyped(src, true, resp)

      (for {
        sfLine <- sfLineNumber
        srcTree <- resp.get.left.toOption
      } yield innerMostEnclIn(srcTree, sfLine)) flatMap {
        case InnerMostEnclTemplate(templDef) =>
          Option(templDef.symbol).filterNot(_ == NoSymbol)
        case _ => None
      }
    }

    def enclosingTemplOf(s: Symbol) = Option(s enclosingSuchThat {e =>
      e != s && (e.isClass || e.isTrait || e.isModuleOrModuleClass || e.isAnonymousFunction)}
    ) filterNot (_ == NoSymbol)

    def isFieldAccessibleAtStackFrame(field: Symbol): Boolean = (for {
      owner <- Option(field.owner).filterNot(_ == NoSymbol)
      sfPosIsWithinOwner <- Try{isStackFrameWithin(owner.pos.asInstanceOf[RangePosition])}.toOption
    } yield sfPosIsWithinOwner) getOrElse false

    def findVariableFromLocalVars(sym: Symbol) = {
      val isLocalVarAccessibleAtStackFrame = (for {
        encloser <- Option(sym.enclosingSuchThat(x => x.isMethod || x.isAnonymousFunction)).filterNot(_ == NoSymbol)
        encloserContainsSfPos <- Try{isStackFrameWithin(encloser.pos.asInstanceOf[RangePosition])}.toOption
      } yield encloserContainsSfPos) getOrElse false

      if(isLocalVarAccessibleAtStackFrame)
        for {
          localVars <- Try{stackFrame.getVariables}.toOption
          foundVar <- localVars.find{_.getName == sym.name.decoded} orElse
            localVars.find{_.getName.split('$')(0) == sym.name.decoded}  // for variables of enclosing methods
        } yield foundVar
      else None
    }

    def findVariableFromFieldsOf(variable: IVariable, sym: Symbol, varMatcher: IVariable => Boolean = null) = {
      def stackFrameCompatibleNameOf(sym: Symbol): String = {  // Based on trial & error. May need more work.
        var name = sym.name.toTermName
        if(sym.isLocalToThis) name = name.dropLocal
        name.decoded
      }

      val variableMatcher = if(varMatcher != null)
        varMatcher
      else
        (v: IVariable) => v.getName == stackFrameCompatibleNameOf(sym)

      for {
        value <- Try{variable.getValue}.toOption
        foundFieldVariable <- value.getVariables.find(variableMatcher)
      } yield foundFieldVariable
    }

    def thisOfStackFrame = Try{stackFrame.getVariables}.toOption flatMap {sfVars =>
      sfVars.find(_.isInstanceOf[ScalaThisVariable]) /* doesn't match in traits */ orElse
        sfVars.find(_.getName == "$this")  // a hack used to access the this when in traits
    }

    def findVariableFromFieldsOfThis(sym: Symbol, varMatcher: IVariable => Boolean = null) =
       thisOfStackFrame flatMap {ths => findVariableFromFieldsOf(ths, sym, varMatcher)}

    // ---------------- END OF HELPERS --------------------------
    /////////////////////////////////////////////////////////////

    Option(t.symbol) flatMap {sym =>
      val isAVariableOrField = sym.isVal || sym.isVar || sym.isAccessor
      if(! isAVariableOrField) None
      else t match {
        case Select(ths: This, _) =>
          def tryGetOuter(inner: IVariable) = for {
            innerVal <- Try{inner.getValue}.toOption
            outer <- innerVal.getVariables.find(_.getName == "$outer")
          } yield outer

          (for {
            x <- innerMostTemplateCertainlyEnclosingSfPos
            y <- Option(ths.symbol)
            z <- thisOfStackFrame
          } yield (x, y, z)) flatMap {case (sfEnclTempl, requestedThisTempl, thisOfSf) =>

            // Symbol equality, except if a param is a module & the other its module class, still returns true.
            def templSymbolsMatch(s1: Symbol, s2: Symbol) =
              s1 == s2 ||
              (s1.isModule && (s1.moduleClass == s2)) ||
              (s2.isModule && (s1 == s2.moduleClass)) ||
              //
              // Only in the test case, sometimes two different Symbol objects are created for the same thing,
              // causing s1 == s2 to be false. The below clause is thus added to make the tests pass.
              s1.fullLocationString == s2.fullLocationString

            if(templSymbolsMatch(requestedThisTempl, sfEnclTempl))
              findVariableFromFieldsOfThis(sym)
            else {
              var currOuter = tryGetOuter(thisOfSf)
              var currEncl = enclosingTemplOf(sfEnclTempl)
              while(currOuter.isDefined && currEncl.isDefined && (! templSymbolsMatch(currEncl.get, requestedThisTempl))) {
                currOuter = tryGetOuter(currOuter.get)
                currEncl = enclosingTemplOf(currEncl.get)
              }
              if(currOuter.isDefined && currEncl.isDefined)
                findVariableFromFieldsOf(currOuter.get, sym)
              else None
            }
          }
        case Select(_, _) => None
        case _ =>
          if(sym.isLocalToBlock) {
            findVariableFromLocalVars(sym) orElse
            //
            // In closures, local vars of enclosing methods are stored as fields with mangled names.
            findVariableFromFieldsOfThis(sym, varMatcher = {_.getName.startsWith(sym.decodedName + "$")})
          } else {
            if(isFieldAccessibleAtStackFrame(sym))
              findVariableFromFieldsOfThis(sym)
            else None
          }
      }
    }
  }
}
