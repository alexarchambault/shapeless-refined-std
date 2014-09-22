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

package shapeless.pimpedstd.ops.traversablelike

import language.higherKinds
import shapeless._, record._
import scala.collection.{ TraversableLike, mutable }
import shapeless.pimpedstd.ops.BuiltOutOf
import shapeless.pimpedstd.util.BuildFrom

/*
 * Same as PartitionAs, but returns both the coproduct element and the original one paired  
 */

trait PartitionWith[A, Repr, C] extends DepFn2[TraversableLike[A, Repr], A => C] with BuiltOutOf[(C, A)] {
  type Out <: HList

  def apply(l: TraversableLike[A, Repr], f: A => C): Out = {
    val b = builder()
    for (x <- l)
      b += f(x) -> x
    b.result()
  }
}

object PartitionWith {
  type Aux[A, Repr, C, Out0] = PartitionWith[A, Repr, C] {type Out = Out0}

  def apply[A, Repr, C] (implicit 
    partitionWith: PartitionWith[C, A, Repr]
  ): Aux[C, A, Repr, partitionWith.Out] = partitionWith

  implicit def cnilPartition[A, Repr]: PartitionWith.Aux[A, Repr, CNil, HNil] =
    new PartitionWith[A, Repr, CNil] {
      type Out = HNil

      def builder() =
        new mutable.Builder[(CNil, A), HNil] {
          def clear(): Unit = ()
          def +=(c: (CNil, A)) = this
          def result() = HNil
        }
    }

  implicit def coproductPartition[K, H, T <: Coproduct, A, Repr] (implicit
    t: PartitionWith[A, Repr, T]
  , cbf: BuildFrom[Repr, (H, A)]
  ): PartitionWith.Aux[A, Repr, FieldType[K, H] :+: T, FieldType[K, cbf.To] :: t.Out] =
    new PartitionWith[A, Repr, FieldType[K, H] :+: T] {
      type Out = FieldType[K, cbf.To] :: t.Out
    
      def builder() =
        new mutable.Builder[(H :+: T, A), FieldType[K, cbf.To] :: t.Out] {
          private val b = cbf()
          private val tailBuilder = t.builder()
    
          def clear(): Unit = {
            b.clear()
            tailBuilder.clear()
          }
    
          def +=(x: (H :+: T, A)) = {
            x match {
              case (Inl(h), a) =>
                b += h -> a
              case (Inr(t), a) =>
                tailBuilder += t -> a
            }
    
            this
          }
    
          def result() = field[K](b.result()) :: tailBuilder.result()
        }
    }
}
