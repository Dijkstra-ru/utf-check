package ru.dijkstra

import scala.collection.mutable.ListBuffer
import scalax.file._
import java.nio._
import java.io.{Closeable, FileInputStream}
import scala._
import org.scalatest.matchers.Matchers._


case class Options(masks: List[String], options: Map[String, String])
case class Sentence(isBOM: Boolean, isValid: Boolean)

class BufferedFileHandle(file: Path) extends Closeable {
  if (!file.exists || !file.isFile)
    throw new Exception("File reading error")
  // Играюсь с размером буффера
  val BUFFER_SIZE = 4096;
  assert(BUFFER_SIZE > 3) // To work with BOM
  val buffer = ByteBuffer allocate(BUFFER_SIZE )
  val channel = (new FileInputStream(file.path)) getChannel()
  var size = channel.read(buffer)
  buffer.flip()
  def next(): Int = {
    if (buffer.position() == BUFFER_SIZE) {
      size = channel.read(buffer)
      buffer.flip()
    }
    if (buffer.position() == size)
      return -1;
    /*
     * Буфер работает странно и мусорит в старших байтах
     * От этого не спасает метод clear
     * Баг подтверждается после переписывания класса без использования pos
     * TODO: сделать что-то человеческое вместо этой байтосодомии
     */
    buffer.get() & 0x000000FF
  }

  /**
   * Read current block again
   */
  def rollback() = buffer.flip()
  override def close() { channel.close() }
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

  def checkFile(handle: BufferedFileHandle) : Sentence = {
    // Find BOM
    val isBOM = if (List(handle.next()),handle.next()),handle.next()) == List(0xef, 0xbb, 0xbf)
  }

  def main(args: Array[String]) {
    val f = Path("src\\test\\resources\\mock")
    val bf = new BufferedFileHandle(f)
    var c = bf.next()
    while (c != -1) {
      println(Integer.toHexString(c))
      c = bf.next()
    }
  }
}
