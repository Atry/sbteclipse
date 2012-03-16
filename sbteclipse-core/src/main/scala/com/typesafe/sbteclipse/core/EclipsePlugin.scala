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

import sbt.{
  Configuration,
  Configurations,
  File,
  Plugin,
  ProjectRef,
  ResolvedProject,
  Setting,
  SettingKey,
  State,
  TaskKey
}
import sbt.Keys.{ baseDirectory, commands }
import scala.util.control.Exception
import scala.xml.{ Attribute, Elem, Null, Text }

object EclipsePlugin extends EclipsePlugin

trait EclipsePlugin {

  def eclipseSettings: Seq[Setting[_]] = {
    import EclipseKeys._
    Seq(
      commandName := "eclipse",
      commands <+= (commandName)(Eclipse.eclipseCommand)
    )
  }

  object EclipseKeys {
    import EclipseOpts._

    val executionEnvironment: SettingKey[Option[EclipseExecutionEnvironment.Value]] =
      SettingKey[Option[EclipseExecutionEnvironment.Value]](
        prefix(ExecutionEnvironment),
        "The optional Eclipse execution environment.")

    val skipParents: SettingKey[Boolean] =
      SettingKey[Boolean](
        prefix(SkipParents),
        "Skip creating Eclipse files for parent project?")

    val withSource: SettingKey[Boolean] =
      SettingKey[Boolean](
        prefix(WithSource),
        "Download and link sources for library dependencies?")

    val classpathEntryTransformerFactory: SettingKey[EclipseClasspathEntryTransformerFactory] =
      SettingKey[EclipseClasspathEntryTransformerFactory](
        prefix("classpathEntryTransformerFactory"),
        "Creates a transformer for classpath entries.")

    val commandName: SettingKey[String] =
      SettingKey[String](
        prefix("command-name"),
        "The name of the command.")

    val configurations: SettingKey[Set[Configuration]] =
      SettingKey[Set[Configuration]](
        prefix("configurations"),
        "The configurations to take into account.")

    val createSrc: SettingKey[EclipseCreateSrc.ValueSet] =
      SettingKey[EclipseCreateSrc.ValueSet](
        prefix("create-src"),
        "The source kinds to be included."
      )

    val eclipseOutput: SettingKey[Option[String]] =
      SettingKey[Option[String]](
        prefix("eclipse-output"),
        "The optional output for Eclipse.")

    val preTasks: SettingKey[Seq[TaskKey[_]]] =
      SettingKey[Seq[TaskKey[_]]](
        prefix("pre-tasks"),
        "The tasks to be evaluated prior to creating the Eclipse project definition."
      )

    val relativizeLibs: SettingKey[Boolean] =
      SettingKey[Boolean](
        prefix("relativize-libs"),
        "Relativize the paths to the libraries?")

    private def prefix(key: String) = "eclipse-" + key
  }

  object EclipseExecutionEnvironment extends Enumeration {

    val JavaSE17 = Value("JavaSE-1.7")

    val JavaSE16 = Value("JavaSE-1.6")

    val J2SE15 = Value("J2SE-1.5")

    val J2SE14 = Value("J2SE-1.4")

    val J2SE13 = Value("J2SE-1.3")

    val J2SE12 = Value("J2SE-1.2")

    val JRE11 = Value("JRE-1.1")

    val valueSeq: Seq[Value] = JavaSE17 :: JavaSE16 :: J2SE15 :: J2SE14 :: J2SE13 :: J2SE12 :: JRE11 :: Nil
  }

  sealed trait EclipseClasspathEntry {
    def toXml: Elem
  }

  object EclipseClasspathEntry {

    case class Src(path: String, output: String) extends EclipseClasspathEntry {
      override def toXml = <classpathentry kind="src" path={ path } output={ output }/>
    }

    case class Lib(path: String, sourcePath: Option[String] = None) extends EclipseClasspathEntry {
      override def toXml =
        sourcePath.foldLeft(<classpathentry kind="lib" path={ path }/>)((xml, sp) =>
          xml % Attribute("sourcepath", Text(sp), Null)
        )
    }

    case class Project(name: String) extends EclipseClasspathEntry {
      override def toXml =
        <classpathentry kind="src" path={ "/" + name } exported="true" combineaccessrules="false"/>
    }

    case class Con(path: String) extends EclipseClasspathEntry {
      override def toXml = <classpathentry kind="con" path={ path }/>
    }

    case class Output(path: String) extends EclipseClasspathEntry {
      override def toXml = <classpathentry kind="output" path={ path }/>
    }
  }

  object EclipseCreateSrc extends Enumeration {

    val Unmanaged = Value

    val Managed = Value

    val Source = Value

    val Resource = Value

    val Default = ValueSet(Unmanaged, Source)

    val All = ValueSet(Unmanaged, Managed, Source, Resource)
  }

  trait EclipseClasspathEntryTransformerFactory {
    def createTransformer(
      ref: ProjectRef,
      state: State): Validation[Seq[EclipseClasspathEntry] => Seq[EclipseClasspathEntry]]
  }

  object EclipseClasspathEntryTransformerFactory {

    object Identity extends EclipseClasspathEntryTransformerFactory {
      import scalaz.Scalaz._
      override def createTransformer(
        ref: ProjectRef,
        state: State): Validation[Seq[EclipseClasspathEntry] => Seq[EclipseClasspathEntry]] =
        ((entries: Seq[EclipseClasspathEntry]) => entries).success
    }

    object Default extends EclipseClasspathEntryTransformerFactory {
      import scalaz.Scalaz._
      override def createTransformer(
        ref: ProjectRef,
        state: State): Validation[Seq[EclipseClasspathEntry] => Seq[EclipseClasspathEntry]] = {
        val transformer =
          (entries: Seq[EclipseClasspathEntry]) => entries collect {
            case EclipseClasspathEntry.Lib(path, _) if path contains "scala-library.jar" =>
              EclipseClasspathEntry.Con("org.scala-ide.sdt.launching.SCALA_CONTAINER")
            case EclipseClasspathEntry.Lib(path, _) if path contains "scala-compiler.jar" =>
              EclipseClasspathEntry.Con("org.scala-ide.sdt.launching.SCALA_COMPILER_CONTAINER")
            case entry =>
              entry
          }
        transformer.success
      }
    }
  }
}
