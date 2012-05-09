package ru.dijkstra
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FreeSpec
import scala._

class ArgParserTest extends FreeSpec with ShouldMatchers {
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
}
