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

import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.refinedstd.ops.{ typelevel, BuiltOutOf }

trait Split[N <: Nat, Sig] extends DepFn2[String, String] with BuiltOutOf[String] {
  def n: Int

  def apply(s: String, regex: String): Out = {
    val b = builder()
    for (x <- s.split(regex, n))
      b += x
    b.result()
  }
}

object Split {
  type Aux[N <: Nat, Sig, Out0] = Split[N, Sig] { type Out = Out0 }
  
  def apply[N <: Nat, Sig] (implicit 
    split: Split[N, Sig]
  ): Aux[N, Sig, split.Out] = split
  
  implicit def split[N <: Nat, Sig] (implicit 
    builder0: typelevel.Builder[Sig, N, String]
  , n0: ToInt[N]
  , ev: N =:!= _0
  ): Aux[N, Sig, builder0.Out] =
    new Split[N, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val n = n0()
    }
}