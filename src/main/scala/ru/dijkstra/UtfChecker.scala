package ru.dijkstra

import scala.collection.mutable.ListBuffer
import scalax.file._
import java.nio._
import java.io.FileInputStream
import scala._


case class Options(masks: List[String], options: Map[String, String])

class BufferedFileHandle(file: Path) {
  if (!file.exists || !file.isFile)
    throw new Exception("File reading error")
  val BUFFER_SIZE = 4096
  val buffer = ByteBuffer allocate(BUFFER_SIZE)
  val channel = (new FileInputStream(file.path)) getChannel()
  var size = channel.read(buffer)
  var pos = 0
  var empty = false
  def next(): Int = {
    if (empty) return -1
    /*
     * Буфер работает странно и мусорит в старших байтах
     * От этого не спасает метод clear
     * TODO: сделать что-то человеческое вместо этой байтосодомии
     */
    val result = buffer.get(pos) & 0x000000FF
    pos += 1
    if (pos == size) {
      pos = 0
      size = channel.read(buffer)
      if (size == -1) {
        empty = true
        channel.close()
      }
    }
    result
  }
}

object UtfChecker {
  def parseArgs(args: List[String]) = {
    var options = Map[String, String]()
    var masks = ListBuffer[String]()
    args.foreach { arg =>
      arg match {
        case option if option.startsWith("-") => {
          val start = if (option.startsWith("--")) 2 else 1
          if (!option.contains("="))
            throw new Exception("Malformed parameter: " + option)
          val key = option.substring(start, option.indexOf("="));
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

  def main(args: Array[String]) {
    val f = Path("c:\\dev\\tex2")
    val bf = new BufferedFileHandle(f)
    var c = bf.next()
    while (c != -1) {
      println(Integer.toHexString(c))
      c = bf.next()
    }
  }
}
