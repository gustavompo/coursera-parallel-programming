import scala.annotation.tailrec

object MergeSort{

  def sort(arr: Array[Int]): Unit = {
    def innerSort( iArr:Array[Int], st:Int, end:Int ): Unit ={
      if(st < end){
        val middle = ((end - st) / 2) + st
        innerSort(iArr, st, middle)
        innerSort(iArr, middle + 1, end)
        merge(iArr, st, middle, end)
      }
    }

    def merge( iArr:Array[Int], st:Int, middle:Int, end:Int )={
      println("Merging" + iArr.toList + " with start [" + st + "] and middle [" + middle + "] and end [" + end +"]" )
      val tmp = new Array[Int](end-st)

      @tailrec
      def innerMerge( pos:Int, lIx:Int, uIx:Int):Unit = {
        println(s"inner pos [$pos], lIx [$lIx], uIx [$uIx]")
        if( lIx >= middle && uIx >= end ) return Unit
        if( iArr(lIx) >= iArr(uIx)  ){
            tmp(pos) = iArr(uIx)
            innerMerge( pos+1, lIx, uIx+1 )
          }else{
          tmp(pos) = iArr(lIx)
          innerMerge(pos+1, lIx+1, uIx)
        }
      }
      innerMerge(0, st, middle)
      //println(tmp.toList)
      var l = 0
      for(ix <- st until end){
        iArr(ix) = tmp(l)
        l = l +1
      }
      val xx = iArr.toList.toString()
      println(s"done iArr[$xx]")
    }
    innerSort(arr,0, arr.length)

//    def parSort(iArr:List[Int], st:Int, end:Int, depth:Int ): Unit = {
//      if (depth < threshold){
//        val middle = ((end - st) / 2) + st
//        parSort(iArr, st, middle, depth + 1)
//        parSort(iArr, middle, end, depth + 1)
//      }else{
//        innerSort(iArr, st, end)
//      }
//    }
//    parSort(arr,0, arr.length, 0)
  }
  val unsorted = Array(5,3,7,4,2,4,67,8)
  sort(unsorted)
  println(unsorted.toList)
}

