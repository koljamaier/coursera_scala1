import funsets.FunSets.{Set, contains}

type ex = Int => Boolean

def func (i: Int) : (Int => Boolean) = {
  i => true
}

type Set = Int => Boolean


def singletonSet(elem: Int): Set = {
  def single(i: Int): Boolean = if(i==elem) true else false
  single
}


func(5)

singletonSet(5)(4)

def union(s: Set, t: Set)(i: Int): Set = {
  def uniF(i : Int) : Boolean = {
    if(contains(t,i) || contains(s,i)) true else false
  }
  uniF
}

union(singletonSet(1), singletonSet(2))