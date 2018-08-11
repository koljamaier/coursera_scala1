package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(Math.pow(b(),2) - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if(delta() < 0) Set()
      else {
        val s1 = ((-1*b()) + (Math.sqrt(delta()))) / (2*a())
        val s2 = ((-1*b()) - (Math.sqrt(delta()))) / (2*a())
        Set(s1,s2)
      }
    }
  }
}
