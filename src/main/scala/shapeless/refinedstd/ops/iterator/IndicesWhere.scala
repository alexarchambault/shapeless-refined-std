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

package shapeless.refinedstd.ops.iterator

import shapeless._
import shapeless.refinedstd.ops.{ typelevel, BuiltOutOf }
import shapeless.ops.nat.ToInt

trait IndicesWhere[Count <: Nat, A, Sig] extends DepFn2[Iterator[A], A => Boolean] with BuiltOutOf[Int] {
  def count: Int

  def apply(it: Iterator[A], p: A => Boolean): Out = {
    val b = builder()
    
    def helper(index: Int, remaining: Int): Unit =
      if (remaining > 0 && it.hasNext) {
        if (p(it.next())) {
          b += index
          helper(index + 1, remaining - 1)
        } else
          helper(index + 1, remaining)
      }
    
    helper(0, count)    
    b.result()
  }
}

object IndicesWhere {
  type Aux[Count <: Nat, A, Sig, Out0] = IndicesWhere[Count, A, Sig] { type Out = Out0 }

  def apply[Count <: Nat, A, Sig] (implicit 
    indicesWhere: IndicesWhere[Count, A, Sig]
  ): Aux[Count, A, Sig, indicesWhere.Out] = indicesWhere

  implicit def indicesWhere[Count <: Nat, A, Sig] (implicit
    builder0: typelevel.Builder[Sig, Count, Int]
  , count0: ToInt[Count]
  ): Aux[Count, A, Sig, builder0.Out] =
      new IndicesWhere[Count, A, Sig] {
        type Out = builder0.Out
        def builder() = builder0()
        val count = count0()
      }
}
