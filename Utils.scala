
object Twister {
  
  private def doDaTwist[A](target:List[A], move:((Int, Int, List[A])=>List[A]), source:List[Int], done:List[Int], to:Int):List[A] = {
    if(source.length==0) {
      target
    }
    else {
      val i = source.head + done.filter(x => x >= source.head).size
      doDaTwist(move(i, to, target), move, source.tail, source.head +: done, to+1)
    }
  }

  def twist[A](target:List[A], source:List[Int], move:((Int, Int, List[A])=>List[A])) = {
    if(target.length != source.length) throw new Error("Lists need to be the same size")
    doDaTwist(target, move, source, List.empty[Int], 0)
  }
}
