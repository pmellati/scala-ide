package org.scalaide.core

import org.eclipse.core.internal.resources.Workspace
import org.eclipse.core.internal.resources.Project
import org.eclipse.core.internal.resources.File
import org.eclipse.core.runtime.Path
import org.eclipse.core.resources.IWorkspace
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.SearchMatch

class EclipseUserSimulator {
  import org.eclipse.jdt.core._

  var root: IPackageFragmentRoot = null
  var workspace: IWorkspace = null

  def createProjectInWorkspace(projectName: String, withSourceRoot: Boolean = true) = {
    import org.eclipse.core.resources.ResourcesPlugin
    import org.eclipse.jdt.internal.core.JavaProject
    import org.eclipse.jdt.core._
    import org.eclipse.jdt.launching.JavaRuntime
    import scala.collection.mutable._

    workspace = ResourcesPlugin.getWorkspace()
    val workspaceRoot = workspace.getRoot()
    val project = workspaceRoot.getProject(projectName)
    project.create(null)
    project.open(null)

    val description = project.getDescription()
    description.setNatureIds(Array(ScalaPlugin.plugin.natureId, JavaCore.NATURE_ID))
    project.setDescription(description, null)

    val javaProject = JavaCore.create(project)
    javaProject.setOutputLocation(new Path("/" + projectName + "/bin"), null)

    val entries = new ArrayBuffer[IClasspathEntry]()
    entries += JavaRuntime.getDefaultJREContainerEntry()

    if (withSourceRoot) {
      val sourceFolder = project.getFolder("/src")
      sourceFolder.create(false, true, null)
      root = javaProject.getPackageFragmentRoot(sourceFolder)
      entries += JavaCore.newSourceEntry(root.getPath())
    }

    entries += JavaCore.newContainerEntry(Path.fromPortableString(ScalaPlugin.plugin.scalaLibId))
    javaProject.setRawClasspath(entries.toArray[IClasspathEntry], null)

    ScalaPlugin.plugin.getScalaProject(project)
  }

  def createPackage(packageName: String): IPackageFragment =
    root.createPackageFragment(packageName, false, null)

  def createCompilationUnit(pack: IPackageFragment, name: String, sourceCode: String) = {
    val cu = pack.createCompilationUnit(name, sourceCode, false, null)
    Thread.sleep(200)
    cu
  }

  def buildWorkspace() {
    import org.eclipse.core.resources.IncrementalProjectBuilder

    workspace.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, new NullProgressMonitor())
  }
}
