package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
//    var cOpen = 0
//    var cClose = 0
//    var x = 0
//    while(x < chars.length){
//      if(chars(x) == '(') cOpen = cOpen + 1
//      if(chars(x) == ')') cClose = cClose + 1
//      x = x+1
//    }
//    return cClose == cOpen
    @tailrec
    def bal(chs: Array[Char], bCount:Int): Boolean = {
      if(bCount < 0) false
      else if(chs.length == 0) bCount == 0
      else {
        val count = if (chs.head == '(') 1 else if (chs.head == ')') -1 else 0
        bal(chs.tail, bCount + count)
      }
    }
    bal(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var ix = idx
      var closingIter = 0
      var openingIter = 0
      while(ix < until ){
        if (chars(ix) == ')'){
          if(openingIter > 0) openingIter = openingIter - 1
          else closingIter = closingIter + 1
        }else if(chars(ix) == '('){
          openingIter = openingIter + 1
        }
        ix = ix+1
      }
      (openingIter, closingIter)
    }

    def reduceOper(a:(Int, Int), b:(Int, Int))={
      val opening = a._1 - b._2 + b._1
      val closing = b._2 - a._1 + a._2
      val res = ( if (opening > 0) opening else 0, if (closing>0) closing else 0 )
      res
    }

    def reduce(from: Int, until: Int, oper: ((Int,Int),(Int,Int))=>(Int,Int)): (Int,Int) = {

        val sectionSize = until - from
        if (sectionSize <= threshold)
          traverse(from, until)
        else {
          val middle = from + sectionSize / 2
          val res = parallel({
            reduce(from, middle, oper)
          }, {
            reduce(middle, until, oper)
          })
          oper(res._1, res._2)
        }

    }

    reduce(0, chars.length, reduceOper) == (0 ,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
