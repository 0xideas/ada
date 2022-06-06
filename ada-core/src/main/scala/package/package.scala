
package object ada {
    trait DoubleValue{
        val value: Double
    }

    abstract class SpecialDouble[A <: DoubleValue](make: Double => A) extends DoubleValue{
        def op(that: Double)(f: (Double, Double) => Double): A = make(f(value, that))

        def +(that: A): A = op(that.value)((d, dd) => d + dd)
        def -(that: A): A = op(that.value)((d, dd) => d - dd)
        def *(that: A): A = op(that.value)((d, dd) => d * dd)
        def /(that: A): A = op(that.value)((d, dd) => d / dd)

        def +(that: Double): A = op(that)((d, dd) => d + dd)
        def -(that: Double): A = op(that)((d, dd) => d - dd)
        def *(that: Double): A = op(that)((d, dd) => d * dd)
        def /(that: Double): A = op(that)((d, dd) => d / dd)

    }

    case class Reward(value: Double) extends SpecialDouble[Reward](d => new Reward(d))

    case class Probability(value: Double) extends SpecialDouble[Probability](d => new Probability(d))

}


