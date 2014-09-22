/*
 * Copyright (c) 2014 Alexandre Archambault
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.pimpedstd.ops.typelevel

import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.pimpedstd.ops.typelevel

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
 * Provides a `Builder` to build a collection made of `N` elements of type `A`. 
 * The collection is typically a type-level one.
 * The result of the `Builder` is an option, which is empty is not enough elements
 * were fed into the mutable.Builder.
 * @tparam From A signature for the type of collection to build, `HList`, `Product`, or `Sized[CC[_]]`,
 *             for HList-s, tuples, or sized-s.
 */
trait Builder[From, N <: Nat, A] {
  type Group
  type Out = Option[Group]
  def apply(): mutable.Builder[A, Out]
}

trait BuilderDefinitions {
  type Aux[From, N <: Nat, A, Group0] = Builder[From, N, A] { type Group = Group0 }

  def apply[Sig, N <: Nat, A] (implicit 
    builder: typelevel.Builder[Sig, N, A]
  ): Aux[Sig, N, A, builder.Group] = builder
}

object Builder extends BuilderDefinitions with BuilderTupleBoilerplate {
  implicit def sizedBuilder[N <: Nat, From, A, Repr] (implicit
    cbf: CanBuildFrom[From, A, Repr],
    ev: AdditiveCollection[Repr],
    n0: ToInt[N]
  ): Aux[Sized[From, N], N, A, Sized[Repr, N]] =
    new Builder[Sized[From, N], N, A] {
      type Group = Sized[Repr, N]

      def apply(): mutable.Builder[A, Out] =
        new mutable.Builder[A, Out] {
          private val b = cbf()
          private var remaining = n0()

          def +=(a: A) = {
            if (remaining > 0) {
              b += a
              remaining -= 1
            }

            this
          }

          def result() =
            if (remaining > 0)
              None
            else
              Some(Sized.wrap[Repr, N](b.result()))

          def clear(): Unit = {
            b.clear()
            remaining = n0()
          }
        }
    }

  implicit def sizedOfBuilder[N <: Nat, From, A, Repr] (implicit
    cbf: CanBuildFrom[From, A, Repr]
  , ev: AdditiveCollection[Repr]
  , n0: ToInt[N]
  ): Aux[SizedOf[From], N, A, Sized[Repr, N]] = {
    val underlying = sizedBuilder[N, From, A, Repr](cbf, ev, n0)
    new Builder[SizedOf[From], N, A] {
      type Group = Sized[Repr, N]

      def apply(): mutable.Builder[A, Out] = underlying()
    }
  }

  trait HListBuilder[N <: Nat, A] extends Builder[HList, N, A] {
    type Group <: HList
  }

  object HListBuilder {
    type Aux[N <: Nat, A, Group0 <: HList] = HListBuilder[N, A] {type Group = Group0}
  }

  implicit def hnilBuilder[A]: HListBuilder.Aux[_0, A, HNil] =
    new HListBuilder[_0, A] {
      type Group = HNil

      def apply(): mutable.Builder[A, Out] =
        new mutable.Builder[A, Out] {
          def +=(a: A) = this
          def result() = Some(HNil)
          def clear() = {}
        }
    }

  implicit def hconsBuilder[N <: Nat, A] (implicit
    tailBuilder: HListBuilder[N, A]
  , n0: ToInt[N]
  ): HListBuilder.Aux[Succ[N], A, A :: tailBuilder.Group] =
    new HListBuilder[Succ[N], A] {
      type Group = A :: tailBuilder.Group

      def apply(): mutable.Builder[A, Out] =
        new mutable.Builder[A, Out] {
          private val b = tailBuilder()
          private var value = Option.empty[A]

          def +=(a: A) = {
            if (value.isEmpty)
              value = Some(a)
            else
              b += a

            this
          }

          def result() =
            for {
              h <- value
              t <- b.result()
            } yield h :: t

          def clear() = {
            value = None
            b.clear()
          }
        }

    }

  implicit def unitBuilder[A]: Aux[Product, _0, A, Unit] =
    new Builder[Product, _0, A] {
      type Group = Unit
      def apply() = new mutable.Builder[A, Out] {
        def clear() = {}
        def +=(x: A) = this
        def result() = Some(())
      }
    }

}

sealed trait SizedOf[Repr]