package ru.dijkstra

import scala.collection.mutable.ListBuffer
import scalax.file.Path
import scalax.file.PathMatcher.GlobPathMatcher

case class Options(masks: List[String], options: Map[String, String])

object UtfChecker {
  def parseArgs(args: List[String]) = {
    var options = Map[String, String]()
    var masks = ListBuffer[String]()
    args.foreach { arg =>
      arg match {
        case option if option.startsWith("-") => {
          if (!option.contains("="))
            throw new Exception("Malformed parameter: " + option)
          val key = option.substring(if (option.startsWith("--")) 2 else 1, option.indexOf("="));
          val value = option.substring(option.indexOf("=") + 1, option.length())
          if (key.length() == 0 || value.length() == 0)
            throw new Exception("Malformed parameter: " + option)
          options += (key -> value)
        }
        case mask => masks += mask
      }
    }
    Options(masks.toList, options)
  }

  def matches (masks: List[String], file: String) =
    if (masks.isEmpty) true
    else if (masks.length == 1) GlobPathMatcher(masks.head)(file)
    else masks map { GlobPathMatcher(_)(file) } reduce (_ || _)

  def main(args: Array[String]) {
    import CheckerOperations.checkFile
    val opt = parseArgs(args.toList)
    val dir = opt.options.get("dir") match {
      case Some(x) => x
      case None => "."
    }
    val path = Path(dir)
    if (path.isFile) {
      val res = checkFile(path)
      if (res.isValid)
        println(path.path + " is valid")
      else
        println(path.path + " is invalid")
      if (res.isBOM)
        println(path.path + " has BOM")
    }
    else {
      var BOMs = 0
      var invalidFiles = 0
      path.descendants() foreach {
        file => if (file.isFile && matches(opt.masks, file.name)) {
          val res = checkFile(file)
          if (res.isBOM) {
            BOMs += 1
            println(file.path + " has BOM")
          }
          if (!res.isValid) {
            invalidFiles += 1
            println(file.path + " is not utf8-correct")
          }
        }
      }
      if (invalidFiles == 0)
        println("All specified files was valid")
      else println("Total invalid files: " + invalidFiles)
      if (BOMs == 0)
        println("No files with BOM found")
      else println("Total files with BOM: " + invalidFiles)
    }
  }
}
