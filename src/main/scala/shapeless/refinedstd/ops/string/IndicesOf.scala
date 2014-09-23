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

package shapeless.refinedstd.ops.string

import shapeless.Nat
import shapeless.ops.nat.ToInt
import shapeless.refinedstd.ops.{ typelevel, BuiltOutOf }
import shapeless.refinedstd.util.DepFn3

trait IndicesOf[N <: Nat, Sig] extends DepFn3[String, String, Int] with BuiltOutOf[Int] {
  def n: Int

  def apply(s: String, str: String, fromIndex: Int): Out = {
    val b = builder()
    
    def helper(idx: Int, remaining: Int): Unit =
      if (remaining > 0) {
        val _idx = s.indexOf(str, idx)
        if (_idx >= 0) {
          b += _idx
          helper(_idx + 1, remaining - 1)
        }
      }
    
    helper(fromIndex, n)
    b.result()
  }

  def char(s: String, ch: Int, fromIndex: Int): Out = {
    val b = builder()

    def helper(idx: Int, remaining: Int): Unit =
      if (remaining > 0) {
        val _idx = s.indexOf(ch, idx)
        if (_idx >= 0) {
          b += _idx
          helper(_idx + 1, remaining - 1)
        }
      }

    helper(fromIndex, n)
    b.result()
  }

  def last(s: String, str: String, fromIndex: Int): Out = {
    val b = builder()

    def helper(idx: Int, remaining: Int): Unit =
      if (remaining > 0) {
        val _idx = s.lastIndexOf(str, idx)
        if (_idx >= 0) {
          b += _idx
          helper(_idx - 1, remaining - 1)
        }
      }

    helper(fromIndex, n)
    b.result()
  }

  def last(s: String, ch: Int, fromIndex: Int): Out = {
    val b = builder()

    def helper(idx: Int, remaining: Int): Unit =
      if (remaining > 0) {
        val _idx = s.lastIndexOf(ch, idx)
        if (_idx >= 0) {
          b += _idx
          helper(_idx - 1, remaining - 1)
        }
      }

    helper(fromIndex, n)
    b.result()
  }
}

object IndicesOf {
  type Aux[N <: Nat, Sig, Out0] = IndicesOf[N, Sig] { type Out = Out0 }

  def apply[N <: Nat, Sig] (implicit 
    indicesOf: IndicesOf[N, Sig]
  ): Aux[N, Sig, indicesOf.Out] = indicesOf

  implicit def indicesOf[N <: Nat, Sig] (implicit 
    builder0: typelevel.Builder[Sig, N, Int]
  , n0: ToInt[N]
  ): Aux[N, Sig, builder0.Out] =
    new IndicesOf[N, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val n = n0()
    }
}