package org.jetbrains.plugins.scala.lang.psi.impl

import com.intellij.ide.scratch.{ScratchFileService, ScratchFileType}
import com.intellij.openapi.fileTypes.LanguageFileType
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.ResolveScopeProvider
import com.intellij.psi.search.GlobalSearchScope
import org.jetbrains.plugins.scala.ScalaLanguage
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaOutOfSourcesResolveScopeProvider._
import org.jetbrains.plugins.scala.project._

class ScalaOutOfSourcesResolveScopeProvider extends ResolveScopeProvider {

  override def getResolveScope(file: VirtualFile, project: Project): GlobalSearchScope =
    file.getFileType match {
      case _: ScratchFileType if isScalaScratchFile(file) =>
        file.scratchFileModule match {
          case Some(module) if module.getProject == project =>
            // scratch file created from one project can be opened in another project manually =/
            GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(module, /*includeTests=*/ false)
          case _ =>
            scalaFileScope(file, project)
        }
      case fileType: LanguageFileType if fileType.getLanguage.isKindOf(ScalaLanguage.INSTANCE) =>
        scalaFileScope(file, project)
      case _ =>
        null
    }

  private def scalaFileScope(file: VirtualFile, project: Project): GlobalSearchScope =
    if (isExternalScalaFile(file, project)) {
      scopeForExternalScalaFile(file, project)
    } else {
      /** rely on [[com.intellij.psi.impl.file.impl.ResolveScopeManagerImpl#getDefaultResolveScope]] */
      null
    }
}

object ScalaOutOfSourcesResolveScopeProvider {

  private def isScalaScratchFile(file: VirtualFile): Boolean =
    ScratchFileService.getInstance.getScratchesMapping.getMapping(file) match {
      case null => false
      case lang => lang.isKindOf(ScalaLanguage.INSTANCE)
    }

  private def isExternalScalaFile(file: VirtualFile, project: Project): Boolean =
    if (project.isDefault) false else {
      val index = ProjectRootManager.getInstance(project).getFileIndex
      val belongsToProject = index != null && (index.isInContent(file) || index.isInSource(file))
      !belongsToProject
    }

  // ensure that we do not have GlobalSearchScope.allScope cause it includes build module jar files
  // (e.g. scala-library.jar with different version)
  private def scopeForExternalScalaFile(file: VirtualFile, project: Project): GlobalSearchScope =
    project.anyScalaModule match {
      case Some(module) =>
        val moduleScope = GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(module, true)
        val fileScope = GlobalSearchScope.fileScope(project, file)
        moduleScope.uniteWith(fileScope)
      case _ =>
        null
    }
}