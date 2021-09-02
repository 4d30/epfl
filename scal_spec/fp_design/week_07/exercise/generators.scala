trait Generator[+T]:
  def generate(): T

  def map[S](f: T => S) = new Generator[S]:
    def generate() = f(Generator.this.generate())

  def flatMap[S](f: T => Generator[S]) = new Generator[S]:
    def generate() = f(Generator.this.generate()).generate()

val integers = new Generator[Int]:
  val rand = java.util.Random()
  def generate() = rand.nextInt()

def single[T](x: T): Generator[T] = new Generator[T]:
  def generate() = x

def range(lo: Int, hi: Int): Generator[Int] = 
  for x <- integers yield  lo + x.abs % (hi - lo)

def oneOf[T](xs: T*): Generator[T] = 
  for idx <- range(0, xs.length) yield xs(idx)



val booleans = integers.map(x => x > 0)

val choice = oneOf("red","green","blue")
val nums = range(6,66)
val a = single(false)


enum Tree:
  case Inner(left: Tree, right: Tree)
  case Leaf(x: Int)

def trees: Generator[Tree] = 
  for
    isLeaf <- range(0, 2)
    tree <- if (isLeaf == 0) then leaves else inners
  yield
    tree

def leaves = 
  for x <- integers yield Tree.Leaf(x)

def inners =
  for x <- trees; y <- trees yield Tree.Inner(x,y)





def main(args: Array[String]): Unit =
  println(trees.generate())
  println(range(0,1).generate())
//  println(integers.generate())



