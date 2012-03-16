/*
 * Copyright 2011 Typesafe Inc.
 *
 * This work is based on the original contribution of WeigleWilczek.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.typesafe.sbteclipse.core

import EclipsePlugin.{
  EclipseClasspathEntry,
  EclipseClasspathEntryTransformerFactory,
  EclipseCreateSrc,
  EclipseExecutionEnvironment,
  EclipseKeys
}
import java.io.{ FileWriter, Writer }
import java.util.Properties
import sbt.{
  Attributed,
  Artifact,
  ClasspathDep,
  Classpaths,
  Command,
  Configuration,
  Configurations,
  File,
  IO,
  Keys,
  ModuleID,
  Project,
  ProjectRef,
  Reference,
  ResolvedProject,
  SettingKey,
  State,
  TaskKey,
  ThisBuild,
  UpdateReport,
  richFile
}
import sbt.complete.Parser
import scala.xml.{ Elem, PrettyPrinter }
import scalaz.{ Failure, NonEmptyList, Success }
import scalaz.Scalaz.{ state => st, _ }
import scalaz.effects._

private object Eclipse {

  val SettingFormat = """-([^:]*):?(.*)""".r

  val FileSep = System.getProperty("file.separator")

  val FileSepPattern = FileSep.replaceAll("""\\""", """\\\\""")

  val JreContainer = "org.eclipse.jdt.launching.JRE_CONTAINER"

  val StandardVmType = "org.eclipse.jdt.internal.debug.ui.launcher.StandardVMType"

  def eclipseCommand(commandName: String): Command =
    Command(commandName)(_ => parser)((state, args) => action(args.toMap, state))

  def parser: Parser[Seq[(String, Any)]] = {
    import EclipseOpts._
    (executionEnvironmentOpt | boolOpt(SkipParents) | boolOpt(WithSource)).*
  }

  def executionEnvironmentOpt: Parser[(String, EclipseExecutionEnvironment.Value)] = {
    import EclipseExecutionEnvironment._
    import EclipseOpts._
    import sbt.complete.DefaultParsers._
    val (head :: tail) = valueSeq map (_.toString)
    val executionEnvironments = tail.foldLeft(head: Parser[String])(_ | _)
    (Space ~> ExecutionEnvironment ~ ("=" ~> executionEnvironments)) map { case (k, v) => k -> withName(v) }
  }

  def action(args: Map[String, Any], state: State): State = {
    state.log.info("About to create Eclipse project files for your project(s).")
    import EclipseOpts._
    handleProjects(
      (args get ExecutionEnvironment).asInstanceOf[Option[EclipseExecutionEnvironment.Value]],
      (args get SkipParents).asInstanceOf[Option[Boolean]] getOrElse skipParents(ThisBuild, state),
      (args get WithSource).asInstanceOf[Option[Boolean]],
      state
    ).fold(onFailure(state), onSuccess(state))
  }

  def handleProjects(
    executionEnvironmentArg: Option[EclipseExecutionEnvironment.Value],
    skipParents: Boolean,
    withSourceArg: Option[Boolean],
    state: State): Validation[IO[Seq[String]]] = {
    val effects = for {
      ref <- structure(state).allProjectRefs
      project <- Project.getProject(ref, structure(state)) if project.aggregate.isEmpty || !skipParents
    } yield {
      val configs = configurations(ref, state)
      val wsource = withSource(ref, state)

      val cetf = classpathEntryTransformerFactory(ref, state).createTransformer(ref, state)
      val n = name(ref, state)
      val buildd = buildDirectory(state)
      val based = baseDirectory(ref, state)
      val srcd = mapConfigurations(configs, srcDirectories(ref, createSrc(ref, state), eclipseOutput(ref, state), state))
      val pd = mapConfigurations(configs, projectDependencies(ref, project, state))

      val foo = for {
        scalacOptions <- st(scalacOptions(ref))
        mc <- mapConfigurations2(configs, externalDependencies(ref, withSourceArg getOrElse wsource))
      } yield {
        val applic = cetf |@|
          n |@|
          buildd |@|
          based |@|
          srcd |@|
          scalacOptions |@|
          mc |@|
          pd
        applic(
          handleProject(
            jreContainer(executionEnvironmentArg orElse executionEnvironment(ref, state)),
            preTasks(ref, state),
            relativizeLibs(ref, state),
            state
          )
        )
      }
      foo ! state
    }
    effects.sequence[Validation, IO[String]] map (_.sequence)
  }

  def onFailure(state: State)(errors: NonEmptyList[String]): State = {
    state.log.error("Could not create Eclipse project files: %s" format (errors.list mkString ", "))
    state.fail
  }

  def onSuccess(state: State)(effects: IO[Seq[String]]): State = {
    val names = effects.unsafePerformIO
    if (names.isEmpty)
      state.log.warn("There was no project to create Eclipse project files for!")
    else
      state.log.info("Successfully created Eclipse project files for project(s): %s" format (names mkString ", "))
    state
  }

  def mapConfigurations[A](
    configurations: Seq[Configuration],
    f: Configuration => Validation[Seq[A]]): Validation[Seq[A]] =
    (configurations map f).sequence map (_.flatten.distinct)

  def mapConfigurations2[A](
    configurations: Seq[Configuration],
    f: Configuration => scalaz.State[State, Validation[Seq[A]]]) = {
    val foo = configurations map f
    val foox = foo.sequence[StateState, Validation[Seq[A]]]
    val fooy = foox map (x => x.sequence map (_.flatten.distinct))
    fooy
  }

  def handleProject(
    jreContainer: String,
    preTasks: Seq[(TaskKey[_], ProjectRef)],
    relativizeLibs: Boolean,
    state: State)(
      classpathEntryTransformer: Seq[EclipseClasspathEntry] => Seq[EclipseClasspathEntry],
      name: String,
      buildDirectory: File,
      baseDirectory: File,
      srcDirectories: Seq[(File, File)],
      scalacOptions: Seq[(String, String)],
      externalDependencies: Seq[Lib],
      projectDependencies: Seq[String]): IO[String] = {
    for {
      _ <- executePreTasks(preTasks, state)
      n <- io(name)
      _ <- saveXml(baseDirectory / ".project", projectXml(name))
      cp <- classpath(
        classpathEntryTransformer,
        buildDirectory,
        baseDirectory,
        relativizeLibs,
        srcDirectories,
        externalDependencies,
        projectDependencies,
        jreContainer,
        state
      )
      _ <- saveXml(baseDirectory / ".classpath", cp)
      _ <- saveProperties(baseDirectory / ".settings" / "org.scala-ide.sdt.core.prefs", scalacOptions)
    } yield n
  }

  def executePreTasks(preTasks: Seq[(TaskKey[_], ProjectRef)], state: State): IO[Unit] =
    //io(for ((preTask, ref) <- preTasks) evaluateTask(preTask, ref, state))
    null

  def projectXml(name: String): Elem =
    <projectDescription>
      <name>{ name }</name>
      <buildSpec>
        <buildCommand>
          <name>org.scala-ide.sdt.core.scalabuilder</name>
        </buildCommand>
      </buildSpec>
      <natures>
        <nature>org.scala-ide.sdt.core.scalanature</nature>
        <nature>org.eclipse.jdt.core.javanature</nature>
      </natures>
    </projectDescription>

  def classpath(
    classpathEntryTransformer: Seq[EclipseClasspathEntry] => Seq[EclipseClasspathEntry],
    buildDirectory: File,
    baseDirectory: File,
    relativizeLibs: Boolean,
    srcDirectories: Seq[(File, File)],
    externalDependencies: Seq[Lib],
    projectDependencies: Seq[String],
    jreContainer: String,
    state: State): IO[Elem] = {
    val srcEntriesIoSeq =
      for ((dir, output) <- srcDirectories) yield srcEntry(baseDirectory, dir, output, state)
    for (srcEntries <- srcEntriesIoSeq.sequence) yield {
      val entries = srcEntries ++
        (externalDependencies map libEntry(buildDirectory, baseDirectory, relativizeLibs, state)) ++
        (projectDependencies map EclipseClasspathEntry.Project) ++
        (Seq(jreContainer) map EclipseClasspathEntry.Con) ++
        (Seq("bin") map EclipseClasspathEntry.Output)
      <classpath>{ classpathEntryTransformer(entries) map (_.toXml) }</classpath>
    }
  }

  def srcEntry(
    baseDirectory: File,
    srcDirectory: File,
    classDirectory: File,
    state: State): IO[EclipseClasspathEntry.Src] =
    io {
      if (!srcDirectory.exists()) srcDirectory.mkdirs()
      EclipseClasspathEntry.Src(
        relativize(baseDirectory, srcDirectory),
        relativize(baseDirectory, classDirectory)
      )
    }

  def libEntry(
    buildDirectory: File,
    baseDirectory: File,
    relativizeLibs: Boolean,
    state: State)(
      lib: Lib): EclipseClasspathEntry.Lib = {
    def path(file: File) = {
      val relativizedBase =
        if (buildDirectory === baseDirectory) Some(".") else IO.relativize(buildDirectory, baseDirectory)
      val relativizedFile = IO.relativize(buildDirectory, file)
      val relativized = (relativizedBase |@| relativizedFile)((base, file) =>
        "%s%s%s".format(
          base split FileSepPattern map (part => if (part != ".") ".." else part) mkString FileSep,
          FileSep,
          file
        )
      )
      if (relativizeLibs) relativized getOrElse file.getAbsolutePath else file.getAbsolutePath
    }
    EclipseClasspathEntry.Lib(path(lib.binary), lib.source map path)
  }

  def jreContainer(executionEnvironment: Option[EclipseExecutionEnvironment.Value]): String =
    executionEnvironment match {
      case Some(ee) => "%s/%s/%s".format(JreContainer, StandardVmType, ee)
      case None => JreContainer
    }

  // Getting and transforming mandatory settings and task results

  def name(ref: Reference, state: State): Validation[String] =
    setting(Keys.name in ref, state)

  def buildDirectory(state: State): Validation[File] =
    setting(Keys.baseDirectory in ThisBuild, state)

  def baseDirectory(ref: Reference, state: State): Validation[File] =
    setting(Keys.baseDirectory in ref, state)

  def target(ref: Reference, state: State): Validation[File] =
    setting(Keys.target in ref, state)

  def srcDirectories(
    ref: Reference,
    createSrc: EclipseCreateSrc.ValueSet,
    eclipseOutput: Option[String],
    state: State)(
      configuration: Configuration): Validation[Seq[(File, File)]] = {
    import EclipseCreateSrc._
    val classDirectory = eclipseOutput match {
      case Some(name) => baseDirectory(ref, state) map (new File(_, name))
      case None => setting(Keys.classDirectory in (ref, configuration), state)
    }
    def dirs(values: ValueSet, key: SettingKey[Seq[File]]) =
      if (values subsetOf createSrc)
        (setting(key in (ref, configuration), state) <**> classDirectory)((sds, cd) => sds map (_ -> cd))
      else
        "".failNel
    Seq(
      dirs(ValueSet(Unmanaged, Source), Keys.unmanagedSourceDirectories),
      dirs(ValueSet(Managed, Source), Keys.managedSourceDirectories),
      dirs(ValueSet(Unmanaged, Resource), Keys.unmanagedResourceDirectories),
      dirs(ValueSet(Managed, Resource), Keys.managedResourceDirectories)
    ) reduceLeft (_ >>*<< _)
  }

  def scalacOptions(ref: ProjectRef)(state: State): (State, Validation[Seq[(String, String)]]) = {
    val (newState, sbtOptions) = evaluateTask(Keys.scalacOptions, ref)(state)
    val options = sbtOptions map (options =>
      if (options.isEmpty) Nil
      else {
        def pluginValues(value: String) =
          value split "," map (_.trim) filterNot (_ contains "org.scala-lang.plugins/continuations")
        options.zipAll(options.tail, "-", "-") collect {
          case (SettingFormat("Xplugin", value), _) if !pluginValues(value).isEmpty =>
            "Xplugin" -> (pluginValues(value) mkString ",")
          case (SettingFormat(key, value), next) if next startsWith "-" =>
            key -> (if (!value.isEmpty) value else "true")
          case (SettingFormat(key, _), next) =>
            key -> next
        } match {
          case Nil => Nil
          case options => ("scala.compiler.useProjectSettings" -> "true") +: options
        }
      }
    )
    newState -> options
  }

  def externalDependencies(
    ref: ProjectRef,
    withSource: Boolean)(
      configuration: Configuration) = {
    def moduleToFile(key: TaskKey[UpdateReport], p: (Artifact, File) => Boolean = (_, _) => true)(state: State) = {
      val (newState, updateReport) = evaluateTask(key in configuration, ref)(state)
      val files = updateReport map { updateReport =>
        val moduleToFile =
          for {
            configurationReport <- (updateReport configuration configuration.name).toSeq
            moduleReport <- configurationReport.modules
            (artifact, file) <- moduleReport.artifacts if p(artifact, file)
          } yield moduleReport.module -> file
        moduleToFile.toMap
      }
      newState -> files
    }
    def libs(files: Seq[Attributed[File]], binaries: Map[ModuleID, File], sources: Map[ModuleID, File]) = {
      val binaryFilesToSourceFiles =
        for {
          (moduleId, binaryFile) <- binaries
          sourceFile <- sources get moduleId
        } yield binaryFile -> sourceFile
      files.files map (file => Lib(file)(binaryFilesToSourceFiles get file))
    }
    def foo(state: State) =
      if (withSource)
        moduleToFile(Keys.updateClassifiers, (artifact, _) => artifact.classifier === Some("sources"))(state)
      else
        state -> Map[ModuleID, File]().success

    for {
      externalDependencyClasspath <- st(evaluateTask(Keys.externalDependencyClasspath in configuration, ref))
      binaryModuleToFile <- st(moduleToFile(Keys.update))
      sourceModuleToFile <- st(foo)
    } yield {
      val externalDependencies =
        (externalDependencyClasspath |@| binaryModuleToFile |@| sourceModuleToFile)(libs)
      //      state.log.debug(
      //        "External dependencies for configuration '%s' and withSource '%s': %s".format(
      //          configuration,
      //          withSource,
      //          externalDependencies
      //        )
      //      )
      externalDependencies
    }
  }

  def projectDependencies(
    ref: ProjectRef,
    project: ResolvedProject,
    state: State)(
      configuration: Configuration): Validation[Seq[String]] = {
    val projectDependencies = project.dependencies collect {
      case dependency if isInConfiguration(configuration, ref, dependency, state) =>
        setting(Keys.name in dependency.project, state)
    }
    val projectDependenciesSeq = projectDependencies.sequence
    state.log.debug("Project dependencies for configuration '%s': %s".format(configuration, projectDependenciesSeq))
    projectDependenciesSeq
  }

  def isInConfiguration(
    configuration: Configuration,
    ref: ProjectRef,
    dependency: ClasspathDep[ProjectRef],
    state: State): Boolean = {
    val map = Classpaths.mapped(
      dependency.configuration,
      Configurations.names(Classpaths.getConfigurations(ref, structure(state).data)),
      Configurations.names(Classpaths.getConfigurations(dependency.project, structure(state).data)),
      "compile", "*->compile"
    )
    !map(configuration.name).isEmpty
  }

  // Getting and transforming optional settings and task results

  def executionEnvironment(ref: Reference, state: State): Option[EclipseExecutionEnvironment.Value] =
    setting(EclipseKeys.executionEnvironment in ref, state).fold(_ => None, id)

  def skipParents(ref: Reference, state: State): Boolean =
    setting(EclipseKeys.skipParents in ref, state).fold(_ => true, id)

  def withSource(ref: Reference, state: State): Boolean =
    setting(EclipseKeys.withSource in ref, state).fold(_ => false, id)

  def classpathEntryTransformerFactory(ref: Reference, state: State): EclipseClasspathEntryTransformerFactory =
    setting(EclipseKeys.classpathEntryTransformerFactory in ref, state).fold(_ =>
      EclipseClasspathEntryTransformerFactory.Default, id
    )

  def configurations(ref: Reference, state: State): Seq[Configuration] =
    setting(EclipseKeys.configurations in ref, state).fold(
      _ => Seq(Configurations.Compile, Configurations.Test),
      _.toSeq
    )

  def createSrc(ref: Reference, state: State): EclipseCreateSrc.ValueSet =
    setting(EclipseKeys.createSrc in ref, state).fold(_ => EclipseCreateSrc.Default, id)

  def eclipseOutput(ref: ProjectRef, state: State): Option[String] =
    setting(EclipseKeys.eclipseOutput in ref, state).fold(_ => None, id)

  def preTasks(ref: ProjectRef, state: State): Seq[(TaskKey[_], ProjectRef)] =
    setting(EclipseKeys.preTasks in ref, state).fold(_ => Seq.empty, _.zipAll(Seq.empty, null, ref))

  def relativizeLibs(ref: ProjectRef, state: State): Boolean =
    setting(EclipseKeys.relativizeLibs in ref, state).fold(_ => true, id)

  // IO

  def saveXml(file: File, xml: Elem): IO[Unit] =
    fileWriter(file).bracket(closeWriter)(writer => io(writer.write(new PrettyPrinter(999, 2) format xml)))

  def saveProperties(file: File, settings: Seq[(String, String)]): IO[Unit] =
    if (!settings.isEmpty) {
      val properties = new Properties
      for ((key, value) <- settings) properties.setProperty(key, value)
      fileWriterMkdirs(file).bracket(closeWriter)(writer =>
        io(properties.store(writer, "Generated by sbteclipse"))
      )
    } else
      io(())

  def fileWriter(file: File): IO[FileWriter] =
    io(new FileWriter(file))

  def fileWriterMkdirs(file: File): IO[FileWriter] =
    io {
      file.getParentFile.mkdirs()
      new FileWriter(file)
    }

  def closeWriter(writer: Writer): IO[Unit] =
    io(writer.close())

  // Utilities

  def relativize(baseDirectory: File, file: File): String =
    IO.relativize(baseDirectory, file).get
}

private case class Content(
  name: String,
  dir: File,
  project: Elem,
  classpath: Elem,
  scalacOptions: Seq[(String, String)])

private case class Lib(binary: File)(val source: Option[File])
