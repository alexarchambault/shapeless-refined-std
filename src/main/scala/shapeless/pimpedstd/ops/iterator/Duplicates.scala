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

package shapeless.pimpedstd
package ops
package iterator

import language.higherKinds
import scala.collection.mutable
import shapeless._
import shapeless.ops.nat.ToInt

trait Duplicates[N <: Nat, A, Sig] extends DepFn1[Iterator[A]] with BuiltOutOf[Iterator[A]] {
  def n: Int

  def apply(it: Iterator[A]): Out = {
    val gaps = Array.fill(n)(new mutable.Queue[A])

    class NthPartner(val idx: Int) extends Iterator[A] {      
      def hasNext: Boolean = 
        gaps(idx).nonEmpty || it.hasNext
      def next(): A = 
        if (gaps(idx).nonEmpty)
          gaps(idx).dequeue()
        else {
          val e = it.next()          
          for (i <- 0 until n if i != idx)
            gaps(i) += e          
          e
        }
      
      // See Iterator.duplicate
      private def compareGaps(otherGaps: Array[mutable.Queue[A]]) = gaps eq otherGaps
      override def hashCode = gaps(idx).hashCode()
      override def equals(other: Any) = other match {
        case x: NthPartner   => x.compareGaps(gaps) && gaps(idx).length == gaps(x.idx).length
        case _               => super.equals(other)
      }
    }

    val b = builder()
    for (idx <- 0 until n)
      b += new NthPartner(idx)
    b.result()
  }
}

object Duplicates {
  type Aux[N <: Nat, A, Sig, Out0] = Duplicates[N, A, Sig] { type Out = Out0 }

  def apply[N <: Nat, A, Sig] (implicit 
    duplicates: Duplicates[N, A, Sig]
  ): Aux[N, A, Sig, duplicates.Out] = duplicates

  implicit def duplicates[N <: Nat, A, Sig] (implicit
    builder0: typelevel.Builder[Sig, N, Iterator[A]]
  , n0: ToInt[N]
  ): Aux[N, A, Sig, builder0.Group] =
    new Duplicates[N, A, Sig] {
      type Out = builder0.Group
      def builder() = builder0().mapResult(_.get)
      val n = n0()
    }
}
