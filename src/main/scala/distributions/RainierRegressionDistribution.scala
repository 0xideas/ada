package epsilon.distributions


import com.stripe.rainier.core.{Distribution => RainierDistribution, _}
import com.stripe.rainier.compute._
import com.stripe.rainier.sampler._

import scala.collection.mutable.{ListBuffer => MutableList}

class RainierRegressionDistribution[Reward <: Double]
    extends ContextualDistribution[Vector[Double], Reward]{

    val contexts: MutableList[(Vector[Double], Reward)] = MutableList()
    private var model: Model = Model.empty
    private var sigma: Real = Exponential(1).latent
    private var mu: Real = Real.Pi

    def draw(context: Vector[Double]): Double = {
        model.optimize(sigma)
    }

    def retrain: Unit = {
        val xs: List[Vector[Double]] = contexts.map(_._1).toList
        val ys: List[Double] = contexts.map(_._2).toList

        sigma = Exponential(1).latent

        val alpha = Normal(0,1).latent
        val betas = Normal(0,1).latentVec(xs(0).length)

        model = Model.observe(ys, Vec.from(xs).map{
            vec => 
            mu = alpha + vec.dot(betas)
            Normal(mu, sigma)
        })
    }

    def update(context: Vector[Double], reward: Reward): Unit = {
        require(contexts.length == 0 || (contexts(0)._1.length == context.length))
        contexts += ((context, reward))
    }
}
