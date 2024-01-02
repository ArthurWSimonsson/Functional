def or(x: Boolean, y: Boolean): Boolean =
    if x then x else y

or(false, false)

def fact(n: Int): Int =
    def factIter(n: Int, acc: Int): Int =
        if n == 0 then acc else factIter(n - 1, n*acc)
    factIter(n,1)

fact(5)

def chain(x: Int): Int => Int = (y: Int) => x + y

chain(5)(7)

def product(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 1 else f(a)*product(f)(a + 1, b)

def factorial(n: Int): Int =
    product(x => x)(1, n)

factorial(4)

def sum(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 0 else f(a) + sum(f)(a + 1, b)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    def recur(a: Int): Int =
        if a > b then zero
        else combine(f(a), recur(a + 1))
    recur(a)

def sumR(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)
def productR(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)

def cube(x: Int): Int = x * x * x


sumR(cube)(1, 5)
sumR(fact)(1, 5)

class Rational(x: Int, y: Int):
    require(y > 0, "denominator must be positive")
    def this(x: Int) = this(x, 1)
    private def gcd(a: Int, b: Int): Int =
        if b == 0 then a else gcd(b, a % b)
    private val g = gcd(x.abs, y)
    val numer = x / g
    val denom = y / g

    def add(r: Rational) =
        Rational(numer * r.denom + r.numer * denom, denom * r.denom)
    def mul(r: Rational) =
        Rational(numer * r.numer, denom * r.denom)
    def neg = Rational(- numer, denom)
    def sub(r: Rational) = add(r.neg)

    def less(that: Rational): Boolean =
        numer * that.denom < that.numer * denom

    infix def max(that: Rational): Rational =
        if this.less(that) then that else this

    override def toString = s"$numer/$denom"
end Rational

extension (r: Rational)
    infix def min(s: Rational) = if s.less(r) then s else r
    infix def abs: Rational = Rational(r.numer.abs, r.denom)
    def + (s: Rational) = r.add(s)
    def * (s: Rational) = r.mul(s)
    def / (s: Rational) = r.mul(s)
    def < (s: Rational) = r.less(s)
    def - (s: Rational) = r.sub(s)

val x = Rational(2,4)
val y = Rational(3,5)
val z = Rational(1,7)

x min y

x + y
x < y
x - y

x.add(y).mul(z)
x.sub(y).sub(z)

x.max(y)
    

abstract class IntSet:
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet

class Empty() extends IntSet:
    def contains(x: Int): Boolean = false
    def incl(x: Int): IntSet = NonEmpty(x, Empty(), Empty())
    def union(s: IntSet): IntSet = s


object IntSet:
    def apply(): IntSet = Empty()
    def apply(x: Int): IntSet = Empty().incl(x)
    def apply(x: Int, y: Int): IntSet = Empty().incl(x).incl(y)


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
    def contains(x: Int): Boolean = 
        if x < elem then left.contains(x)
        if x > elem then right.contains(x)
        else true

    def incl(x: Int): IntSet =
        if x < elem then NonEmpty(elem, left.incl(x), right)
        if x > elem then NonEmpty(elem, left, right.incl(x))
        else this

    def union(s: IntSet): IntSet = 
        left.union(right).union(s).incl(elem)

end NonEmpty


trait Planar: //used instead of abstract to extend several supertypes
    def height: Int
    def width: Int
    def surface = height * width

trait List[T]:
    def isEmpty: Boolean
    def head: T
    def tail: List[T]

class Cons[T](val head: T, val tail: List[T]) extends List[T]:
    def isEmpty = false

class Nil[T] extends List[T]:
    def isEmpty = true
    def head = throw new NoSuchElementException("Nil.head")
    def tail = throw new NoSuchElementException("Nil.tail")

def nth[T](xs: List[T], n: Int): T = 
    if xs.isEmpty then throw IndexOutOfBoundsException()
    else if n == 0 then xs.head
    else nth(xs.tail, n - 1)
