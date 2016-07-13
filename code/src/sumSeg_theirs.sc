package week1.worklog

import scala.collection.mutable.ListBuffer

object sumSeg_theirs {

	def power( x:Int, p:Double ):Int = math.exp( p * math.log( math.abs( x ) ) ).toInt
	def pNorm ( ar:Array[Int], p:Double ) = power( sumSeg( ar, p, 0, ar.length - 1 ), 1 / p )

	def sumSeg(a:Array[Int], p:Double, s:Int, t:Int): Int = {
		var i = s;
		var sum:Int = 0
		while( i < t ){
			sum = sum + power( a( i ), p ).toInt
			i = i + 1
		}
		sum
	}
	
	def pNormTwoPart (a:Array[Int], p: Double):Int = {
		val m = a.length / 2
		val (sum1, sum2) = parallel(sumSeg(a, p, 0, m), sumSeg(a, p, m, a.length))
		power(sum1 + sum2, 1/p)
	}
}