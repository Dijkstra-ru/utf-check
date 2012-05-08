package ru.dijkstra
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FreeSpec
import scala.collection.mutable.ListBuffer
import scalax.file.Path
import java.nio._
import java.io.FileOutputStream
import scala._
import scala.util.control.Breaks._

class Test  extends FreeSpec with ShouldMatchers {
  "Arg parser test" - {
    "  No args " in {
      val a = UtfChecker.parseArgs(List())
      a.masks should have length 0
      a.options.toList should have length 0
    }
    "  Sole mask " in {
      val a = UtfChecker.parseArgs(List("*.*"))
      a.masks should be === List("*.*")
      a.options.toList should have length 0
    }
    "  --Dir option" in {
      val a = UtfChecker.parseArgs(List("--dir=c:\\dev"))
      a.masks should have length 0
      a.options should be === Map {"dir" -> "c:\\dev" }
    }
    "  -Dir option" in {
      val a = UtfChecker.parseArgs(List("-dir=c:\\dev"))
      a.masks should have length 0
      a.options should be === Map {"dir" -> "c:\\dev" }
    }
    "  Mixed info" in {
      val a = UtfChecker.parseArgs(List("*.cpp","--dir=c:\\dev","*.h"))
      a.masks should be === List("*.cpp", "*.h")
      a.options should be === Map {"dir" -> "c:\\dev" }
    }
    "  No eq option 1" in {
      val thrown = evaluating {
        UtfChecker.parseArgs(List("-dir"))
      } should produce [Exception]
      thrown.getMessage should equal ("Malformed parameter: -dir")
    }
    "  No eq option 2" in {
      val thrown = evaluating {
        UtfChecker.parseArgs(List("--dir"))
      } should produce [Exception]
      thrown.getMessage should equal ("Malformed parameter: --dir")
    }
    "  Empty value in option" in {
      val thrown = evaluating {
        UtfChecker.parseArgs(List("-dir="))
      } should produce [Exception]
      thrown.getMessage should equal ("Malformed parameter: -dir=")
    }
    "  Empty key in option" in {
      val thrown = evaluating {
        UtfChecker.parseArgs(List("-="))
      } should produce [Exception]
      thrown.getMessage should equal ("Malformed parameter: -=")
    }
  }
  "Byte reader test" - {
    "  Generic byte sequence less than 4kb" in {
    val f = Path("src\\test\\resources\\mock")
    val bf = new BufferedFileHandle(f)
    var lb = ListBuffer[Int]()
    lb += bf.next()
    while (lb.last != -1) lb += bf.next()
    lb.toList should be === List(0xef, 0xbb, 0xbf, 0x61, 0xd1, 0x84, 0x32, 0x33, -1)
    }
    // TODO: какой-то сомнительный тест я накорябал
    // Хотя он не проходит на других имеющихся образцах
    "  Generic byte sequence greater than 4kb" in {
      val f = Path("src\\test\\resources\\mock5k")
      // Файл содержит последовательности байтов 1, 2, 3, 4, 5
      val bf = new BufferedFileHandle(f)
      breakable {
        var byte = 0
        var prev_byte = 0
        while(true) {
          prev_byte = byte
          byte = bf.next()
          if (byte == -1)
            break()
          // Если последовательность нарушена, то буффер работает с ошибкой
          if (!((byte == 1 && (prev_byte == 5 || prev_byte == 0)) ||
            byte - prev_byte == 1))
            throw new Exception("Bad buffer " + byte + " " + prev_byte)
        }
      }
    }
  }

  // generateTestFile("src\\test\\resources\\mock5k")
  def generateTestFile(filename: String) {
    val channel = (new FileOutputStream(filename)) getChannel()
    val buffer = ByteBuffer allocate(5)
    buffer.put(1: Byte).put(2: Byte).put(3: Byte).put(4: Byte).put(5: Byte)
    for (i <- 1 until 1000) {
      buffer.flip()
      channel.write(buffer)
    }
    channel.close()
  }
}