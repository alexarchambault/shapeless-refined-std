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

package shapeless.refinedstd.ops.traversablelike

import language.higherKinds
import shapeless._, record._
import scala.collection.{ TraversableLike, mutable }
import shapeless.refinedstd.ops.BuiltOutOf
import shapeless.refinedstd.util.BuildFrom

/*
 * Inspired by @travisbrown's blog post and SO answer
 *   http://meta.plasm.us/posts/2014/06/14/partitioning-by-constructor/
 *   http://stackoverflow.com/questions/24210197/type-based-collection-partitioning-in-scala/24220112#24220112
 */

trait PartitionAs[A, Repr, C] extends DepFn2[TraversableLike[A, Repr], A => C] with BuiltOutOf[C] {
  type Out <: HList

  def apply(l: TraversableLike[A, Repr], as: A => C): Out = {
    val b = builder()
    for (x <- l)
      b += as(x)
    b.result()
  }
}

object PartitionAs {
  type Aux[A, Repr, C, Out0] = PartitionAs[A, Repr, C] {type Out = Out0}

  def apply[A, Repr, C] (implicit 
    partitionAs: PartitionAs[A, Repr, C]
  ): Aux[A, Repr, C, partitionAs.Out] = partitionAs

  implicit def cnilPartition[A, Repr]: PartitionAs.Aux[A, Repr, CNil, HNil] =
    new PartitionAs[A, Repr, CNil] {
      type Out = HNil

      def builder() =
        new mutable.Builder[CNil, HNil] {
          def clear(): Unit = ()
          def +=(c: CNil) = this
          def result() = HNil
        }
    }

  implicit def coproductPartition[K, H, T <: Coproduct, A, Repr] (implicit
    t: PartitionAs[A, Repr, T]
  , cbf: BuildFrom[Repr, H]
  ): PartitionAs.Aux[A, Repr, FieldType[K, H] :+: T, FieldType[K, cbf.To] :: t.Out] =
    new PartitionAs[A, Repr, FieldType[K, H] :+: T] {
      type Out = FieldType[K, cbf.To] :: t.Out
   
      def builder() =
        new mutable.Builder[H :+: T, FieldType[K, cbf.To] :: t.Out] {
          private val b = cbf()
          private val tailBuilder = t.builder()
   
          def clear(): Unit = {
            b.clear()
            tailBuilder.clear()
          }
   
          def +=(x: H :+: T) = {
            x match {
              case Inl(h) =>
                b += h
              case Inr(t) =>
                tailBuilder += t
            }
   
            this
          }
   
          def result() = field[K](b.result()) :: tailBuilder.result()
        }
    }
  
}
