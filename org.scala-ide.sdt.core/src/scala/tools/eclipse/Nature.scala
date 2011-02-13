/*
 * Copyright 2005-2010 LAMP/EPFL
 * @author Sean McDirmid
 */
// $Id$

package scala.tools.eclipse

import scala.collection.mutable.ArrayBuffer

import org.eclipse.core.resources.{ ICommand, IProject, IProjectNature, IResource }
import org.eclipse.jdt.core.{ IClasspathEntry, IJavaProject, JavaCore }
import org.eclipse.jdt.launching.JavaRuntime
import org.eclipse.core.runtime.Path

class Nature extends IProjectNature {
  protected def plugin : ScalaPlugin = ScalaPlugin.plugin
  
  private var project : IProject = _

  override def getProject = project
  override def setProject(project : IProject) = this.project = project
  
  override def configure() {
    if (project == null || !project.isOpen)
      return
    
    updateBuilders(project, List(JavaCore.BUILDER_ID, plugin.oldBuilderId), plugin.builderId)
    
    plugin.check {
      val jp = JavaCore.create(getProject)
      removeClasspathContainer(jp)
      val buf = ArrayBuffer(jp.getRawClasspath : _*)
      
      // Put the Scala classpath container before JRE container
      val scalaLibEntry = JavaCore.newContainerEntry(Path.fromPortableString(plugin.scalaLibId))
      val jreIndex = buf.indexWhere(_.getPath.toPortableString.startsWith(JavaRuntime.JRE_CONTAINER))
      if (jreIndex != -1) {
        buf.insert(jreIndex, scalaLibEntry)
      } else {
        buf += scalaLibEntry
        buf += JavaCore.newContainerEntry(Path.fromPortableString(JavaRuntime.JRE_CONTAINER))
      }
      jp.setRawClasspath(buf.toArray, null)
      jp.save(null, true)
    }
  }
  
  override def deconfigure() {
    if (project == null || !project.isOpen)
      return

    updateBuilders(project, List(plugin.builderId, plugin.oldBuilderId), JavaCore.BUILDER_ID)

    val jp = JavaCore.create(getProject)
    removeClasspathContainer(jp)
    jp.save(null, true)
  }
  
  private def removeClasspathContainer(jp : IJavaProject) {
    val scalaLibPath = Path.fromPortableString(plugin.scalaLibId)
    val oldScalaLibPath = Path.fromPortableString(plugin.oldScalaLibId)
    val buf = jp.getRawClasspath filter { entry => { val path = entry.getPath ; path != scalaLibPath && path != oldScalaLibPath  } }
    jp.setRawClasspath(buf, null)
  }
  
  private def updateBuilders(project: IProject, buildersToRemove: List[String], builderToAdd: String) {
    plugin.check {
      val description = project.getDescription
      val previousCommands = description.getBuildSpec
      val filteredCommands = previousCommands.filterNot(buildersToRemove contains _.getBuilderName)
      val newCommands = if (filteredCommands.exists(_.getBuilderName == builderToAdd))
        filteredCommands
      else
        filteredCommands :+ { 
          val newBuilderCommand = description.newCommand;
          newBuilderCommand.setBuilderName(builderToAdd);
          newBuilderCommand
        }
      description.setBuildSpec(newCommands)
      project.setDescription(description, IResource.FORCE, null)
    }
  }
}
