abstract class GeneratingFunction {

  def isZero: Boolean

  def head: BigInt

  def tail: GeneratingFunction

  protected def mkString: String

  override def toString: String = "(" + mkString + ")"

  def take(n: Int): List[BigInt] =
    if (n == 0) Nil
    else this.head :: this.tail.take(n - 1)

  def + (that: GeneratingFunction): GeneratingFunction

  def unary_- : GeneratingFunction

  def - (that: GeneratingFunction): GeneratingFunction

  def * (that: GeneratingFunction): GeneratingFunction

  def / (that: GeneratingFunction): GeneratingFunction

  def compose(that: GeneratingFunction): GeneratingFunction

  def revert: GeneratingFunction

  def derivative: GeneratingFunction

  def integral: GeneratingFunction

  def ordinaryToExponential: GeneratingFunction

  def exponentialToOrdinary: GeneratingFunction

}


object GeneratingFunction {

  object Zero extends GeneratingFunction {

    override def isZero = true

    override def head = 0

    override def tail = Zero

    override protected def mkString = "0, 0, 0, ..."

    override def + (that: GeneratingFunction) = that

    override def unary_- = Zero

    override def - (that: GeneratingFunction) = -that

    override def * (that: GeneratingFunction) = Zero

    override def / (that: GeneratingFunction) =
      if (that.isZero) throw new RuntimeException("0/0 is not defined")
      else Zero

    override def compose(that: GeneratingFunction) = Zero

    override def revert = throw new RuntimeException("Zero.revert")

    override def derivative = Zero

    override def integral = Zero

    override def ordinaryToExponential = Zero

    override def exponentialToOrdinary = Zero

  }


  final class Series(hd: BigInt, tl: => GeneratingFunction) extends GeneratingFunction {

    override def isZero = false

    override def head = hd

    override def tail = tl

    override protected def mkString = head + ", " + tail.mkString

    override def + (that: GeneratingFunction) =
      if (that.isZero) this
      else new Series(this.head + that.head, this.tail + that.tail)

    override def unary_- = new Series(-head, -tail)

    override def - (that: GeneratingFunction) =
      if (that.isZero) this
      else new Series(this.head - that.head, this.tail - that.tail)

    override def * (that: GeneratingFunction) =
      if (that.isZero) Zero
      else if (this.head == 0) new Series(0, this.tail * that)
      else new Series(this.head * that.head, (this.head * that.tail) + (this.tail * that))

    override def / (that: GeneratingFunction) =
      if (this.head == 0 && that.head == 0) this.tail / that.tail
      else if (this.head == 0) new Series(0, this.tail / that)
      else if (that.head == 0) throw new RuntimeException("division by 0 attempted")
      else {
        val q = this.head / that.head
        new Series(q, (this.tail - that.tail * q) / that)
      }

    override def compose(that: GeneratingFunction) =
      if (that.isZero) this.head
      else if (that.head == 0) new Series(this.head, that.tail * this.tail.compose(that))
      else throw new RuntimeException("head of the argument must be zero")

    override def revert =
      if (this.head == 0) {
        new Series(0, BigInt(1) / this.tail.compose(this.revert))
      }
      else throw new RuntimeException("this.head must be zero")

    override def derivative = {
      def iter(gf: GeneratingFunction, n: Int): GeneratingFunction =
        if (gf.isZero) Zero
        else new Series(n * gf.head, iter(gf.tail, n + 1))
      iter(this.tail, 1)
    }

    override def integral = {
      def iter(gf: GeneratingFunction, n: Int): GeneratingFunction =
        if (gf.isZero) Zero
        else new Series(gf.head / n, iter(gf.tail, n + 1))
      new Series(0, iter(this, 1))
    }

    override def ordinaryToExponential =
      new Series(this.head, this.derivative.ordinaryToExponential)

    override def exponentialToOrdinary =
      new Series(this.head, this.tail.exponentialToOrdinary.integral.tail)

  }


  class SeriesWrapper(tl: => GeneratingFunction) {
    def +::(hd: BigInt): GeneratingFunction = new Series(hd, tl)
  }

  implicit def seriesWrapper(gf: => GeneratingFunction): SeriesWrapper = new SeriesWrapper(gf)

  object +:: {
    def unapply(gf: GeneratingFunction): Option[(BigInt, GeneratingFunction)] =
      if (gf.isZero) Some(0, GeneratingFunction.Zero)
      else Some((gf.head, gf.tail))
  }

  implicit def fromBigInt(a: BigInt): GeneratingFunction = new Series(a, Zero)


  def apply(as: BigInt*): GeneratingFunction =
    if (as.isEmpty) Zero
    else as.head +:: apply(as.tail:_*)

  def zero: GeneratingFunction = Zero

  val z: GeneratingFunction = 0 +:: 1 +:: Zero

}