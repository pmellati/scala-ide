package org.scalaide.refactoring.internal

import scala.tools.refactoring.common.TextChange
import scala.tools.refactoring.implementations

import org.eclipse.ui.PlatformUI
import org.scalaide.core.internal.jdt.model.ScalaSourceFile
import org.scalaide.util.internal.eclipse.EditorUtils

/**
 * From a selected expression, the Extract Local refactoring will create a new
 * value in the closest enclosing scope and replace the selected expression with
 * a reference to that value.
 *
 * Extract Local also uses Eclipse's linked UI mode.
 */
class ExtractLocal extends RefactoringExecutor {

  def createRefactoring(selectionStart: Int, selectionEnd: Int, file: ScalaSourceFile) =
    new ExtractLocalScalaIdeRefactoring(selectionStart, selectionEnd, file)

  class ExtractLocalScalaIdeRefactoring(start: Int, end: Int, file: ScalaSourceFile)
      extends ScalaIdeRefactoring("Extract Local", file, start, end) {

    val refactoring = withCompiler( c => new implementations.ExtractLocal { val global = c })

    val name = "extractedLocalValue"

    def refactoringParameters = name
  }

  override def perform(): Unit = {

    /**
     * Inline extracting is implemented by extracting to a new name
     * that does not exist and then looking up the position of these
     * names in the generated change.
     */
    def doInlineExtraction(change: TextChange, name: String) {
      EditorUtils.doWithCurrentEditor { editor =>

        EditorUtils.applyRefactoringChangeToEditor(change, editor)

        val occurrences = {
          val firstOccurrence  = change.text.indexOf(name)
          val secondOccurrence = change.text.indexOf(name, firstOccurrence + 1)
          List(firstOccurrence, secondOccurrence) map (o => (change.from + o, name.length))
        }

        EditorUtils.enterLinkedModeUi(occurrences, selectFirst = true)
      }
    }

    val shell = PlatformUI.getWorkbench.getActiveWorkbenchWindow.getShell

    createScalaIdeRefactoringForCurrentEditorAndSelection() match {
      case Some(r: ExtractLocalScalaIdeRefactoring) =>

        r.preparationResult.right.map(_ => r.performRefactoring()) match {
          case Right((change: TextChange) :: Nil) =>
            doInlineExtraction(change, r.name)
          case _ =>
            runRefactoring(createWizardForRefactoring(Some(r)), shell)
        }

      case None => runRefactoring(createWizardForRefactoring(None), shell)
    }
  }
}
