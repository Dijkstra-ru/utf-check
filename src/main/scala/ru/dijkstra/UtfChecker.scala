package ru.dijkstra

import scala.collection.mutable.ListBuffer
import scalax.file._
import java.nio._
import java.io.FileInputStream
import scala.Predef._
import scala._


case class Options(masks: List[String], options: Map[String, String])
case class Sentence(isBOM: Boolean, isValid: Boolean)

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

  def checkFile(file: Path) : Sentence = {
    if (!file.exists || !file.isFile)
      throw new Exception("File reading error")
    val BUFFER_SIZE = 4096;
    val buffer = ByteBuffer.allocate(BUFFER_SIZE)
    val channel = (new FileInputStream(file.path)) getChannel()
    val size = channel.read(buffer)
    buffer.flip()

    0x01
    val isBOM = if (size < 3) false
                else (List(buffer.get(), buffer.get(), buffer.get())
                   == List(-17: Byte, -69: Byte, -65: Byte)) // EF BB BF
    if (!isBOM) buffer.rewind()
    var isValid = true
    try {
      var eof = false
      do {
        if (buffer.remaining() < 6 || eof) {
          buffer.compact()
          val i = channel.read(buffer)
          buffer.flip()
          eof = i == -1
        }
        check(buffer)
      } while (!eof)
    } catch {
      case _ => isValid = false
    }
    Sentence(isBOM, isValid)
  }

  object u8Len {
    def unapply(c: Byte) = {
      if (((c ^ 0x80) | 0x7f) == 0xff) Some(1)
      else if (((c ^ 0x40) | 0x3f) == 0xff) Some(2)
      else if (((c ^ 0x20) | 0x1f) == 0xff) Some(3)
      else if (((c ^ 0x10) | 0x0f) == 0xff) Some(4)
      else if (((c ^ 0x08) | 0x07) == 0xff) Some(5)
      else if (((c ^ 0x04) | 0x03) == 0xff) Some(6)
      else throw new Exception()
    }
  }

  def check(in: ByteBuffer) {
    val b = in.get()
    b match {
      case u8Len(l) => checkData(l - 1, in)
      case _ => throw new Exception()
    }
  }

  def checkData(length: Int, in: ByteBuffer) {
    for (i <- 0 until length) {
      if (((in.get() & 0xC0) /*11000000*/  >> 6) != 2 /*10*/)
        throw new Exception()
    }
  }

  def main(args: Array[String]) {
    val res = UtfChecker.checkFile(Path("src\\test\\resources\\mock"))
    println(res.isBOM)
    println(res.isValid)

  }
}
