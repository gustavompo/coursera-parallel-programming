package week1.worklog

import scala.collection.mutable.ListBuffer

object SumSegmentLiteral {
	def sum ( arr:Array[Int], p:Double, s:Int, t:Int ):Int = {
	
		var exps = for(i <- s to t) yield power(arr(i), p);
		println(exps);
		exps.foldLeft(0) { (acc, curr) =>
			(acc + curr).toInt
		}
	}                                         //> sum: (arr: Array[Int], p: Double, s: Int, t: Int)Int
	def powerr(x:Int, p:Double):Int = math.exp(p*math.log(math.abs(x))).toInt
                                                  //> powerr: (x: Int, p: Double)Int
	def power(a:Double, b:Double) = Math.pow(a, b)
                                                  //> power: (a: Double, b: Double)Double
	def pNorm (ar:Array[Int], p:Double) = powerr(sum(ar, p, 0, ar.length-1), 1/p)
                                                  //> pNorm: (ar: Array[Int], p: Double)Int
	
	val a = Array( 1, 2, 3, 4, 5, 6 );        //> a  : Array[Int] = Array(1, 2, 3, 4, 5, 6)
	println(sum(a, 1, 1, 1))                  //> Vector(2.0)
                                                  //| 2
	println(pNorm(a, 2))                      //> Vector(1.0, 4.0, 9.0, 16.0, 25.0, 36.0)
                                                  //| 9
	
	 
	def pNormParallel(ar:Array[Int], p:Double, parallelLvl:Int) = {
		val locker = new AnyRef
		var sums = 0
		val threads = List.range(1, parallelLvl).map(i => {
			val begin = (i * (ar.length / parallelLvl)).toInt
			var end = ((i + 1) * (ar.length / parallelLvl)).toInt;
			end = if (end >= ar.length) ar.length-1 else end
			
			val t = new Thread {
				override def run(){
					val res = sum(ar, p, begin, end)
					locker.synchronized{
						sums += res
					}
				}
			}
			t.start
			t
		});
		
		threads.foreach(t => t.join);
		power( sums, 1/p)
		}                                 //> pNormParallel: (ar: Array[Int], p: Double, parallelLvl: Int)Double
		
		println(pNormParallel(a, 2, 4))   //> Vector(4.0, 9.0)
                                                  //| Vector(9.0, 16.0)
                                                  //| Vector(16.0, 25.0)
                                                  //| 8.888194417315589
		//threads.each start
	//}
	
	def sumSeg(a:Array[Int], p:Double, s:Int, t:Int): Int ={
		var i=s; var sum:Int=0
		while(i<t){
		sum = sum + powerr(a(i),p).toInt
		i=i+1
		}
		sum
	}                                         //> sumSeg: (a: Array[Int], p: Double, s: Int, t: Int)Int
	
	println(sum(a, 1, 1, 1))                  //> Vector(2.0)
                                                  //| 2
	println(sumSeg(a, 1, 1, 1))               //> 0
	
	println(for(x <- 0 to 10) yield x)        //> Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
}