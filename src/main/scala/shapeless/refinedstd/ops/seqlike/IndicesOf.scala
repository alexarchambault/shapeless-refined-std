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

package shapeless.refinedstd.ops.seqlike

import scala.collection.SeqLike
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.refinedstd.ops.{ typelevel, BuiltOutOf }
import shapeless.refinedstd.util.DepFn3

trait IndicesOf[Count <: Nat, A, Repr, B >: A, Sig] extends DepFn3[SeqLike[A, Repr], B, Int] with BuiltOutOf[Int] {
  def count: Int

  def apply(coll: SeqLike[A, Repr], elem: B, fromIndex: Int): Out = {
    val b = builder()
    var idx = fromIndex
    val length = coll.length
    var found = 0

    while (found < count && idx < length) {
      val sliceIdx = coll.indexOf(elem, idx)
      if (sliceIdx < 0) {
        idx = length
      } else {
        b += sliceIdx
        found += 1
        idx = sliceIdx + 1
      }
    }

    b.result()
  }

  def last(coll: SeqLike[A, Repr], elem: B, fromIndex: Int): Out = {
    val b = builder()
    var idx = fromIndex
    var found = 0

    while (found < count && idx >= 0) {
      val sliceIdx = coll.lastIndexOf(elem, idx)
      if (sliceIdx < 0) {
        idx = -1
      } else {
        b += sliceIdx
        found += 1
        idx = sliceIdx - 1
      }
    }

    b.result()
  }
}

object IndicesOf {
  type Aux[Count <: Nat, A, Repr, B >: A, Sig, Out0] = IndicesOf[Count, A, Repr, B, Sig] { type Out = Out0 }

  def apply[Count <: Nat, A, Repr, B >: A, Sig] (implicit 
    indicesOf: IndicesOf[Count, A, Repr, B, Sig]
  ): Aux[Count, A, Repr, B, Sig, indicesOf.Out] = indicesOf

  implicit def indicesOf[Count <: Nat, A, Repr, B >: A, Sig] (implicit 
    builder0: typelevel.Builder[Sig, Count, Int]
  , count0: ToInt[Count]
  ): Aux[Count, A, Repr, B, Sig, builder0.Out] =
    new IndicesOf[Count, A, Repr, B, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val count = count0()
    }
}
