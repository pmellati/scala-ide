package org.scalaide.ui.editor.extensionpoints

import org.eclipse.core.runtime.Platform
import org.eclipse.jface.text.ITextHover
import org.scalaide.core.internal.jdt.model.ScalaCompilationUnit
import org.scalaide.util.internal.Utils

object ScalaHoverDebugOverrideExtensionPoint {
  final val EXTENSION_POINT_ID = "org.scala-ide.sdt.core.scalaHoverDebugOverride"

  def hoverFor(scu: ScalaCompilationUnit): Option[ITextHover] = extensionHoverFactory map {_ createFor scu}

  private lazy val extensionHoverFactory: Option[TextHoverFactory] = for {
    configElem <- Platform.getExtensionRegistry.getConfigurationElementsFor(EXTENSION_POINT_ID).headOption  // A max of 1 extension is allowed.
    f <- Utils.tryExecute(configElem.createExecutableExtension("hoverFactoryClass")) collect {case f: TextHoverFactory => f}
  } yield f
}

trait TextHoverFactory {
  def createFor(scu: ScalaCompilationUnit): ITextHover
}