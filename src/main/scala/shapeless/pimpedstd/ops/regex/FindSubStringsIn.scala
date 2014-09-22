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

package shapeless.pimpedstd.ops.regex

import scala.util.matching.Regex
import shapeless.{ DepFn2, Nat }
import shapeless.ops.nat.ToInt
import shapeless.pimpedstd.ops.{ typelevel, BuiltOutOf }


trait FindSubStringsIn[N <: Nat, Sig] extends DepFn2[Regex, CharSequence] with BuiltOutOf[String] {
  def n: Int

  def apply(regex: Regex, toSplit: CharSequence): Out = {
    val b = builder()
    for (s <- regex.findAllIn(toSplit).take(n))
      b += s
    b.result()
  }
}

object FindSubStringsIn {
  type Aux[N <: Nat, Sig, Out0] = FindSubStringsIn[N, Sig] { type Out = Out0 }
  
  def apply[N <: Nat, Sig] (implicit 
    findSubStringsIn: FindSubStringsIn[N, Sig]
  ): Aux[N, Sig, findSubStringsIn.Out] = findSubStringsIn

  implicit def findSubStringsIn[N <: Nat, Sig] (implicit 
    builder0: typelevel.Builder[Sig, N, String] 
  , n0: ToInt[N]
  ): Aux[N, Sig, builder0.Out] =
    new FindSubStringsIn[N, Sig] {
      type Out = builder0.Out
      def builder() = builder0()
      val n = n0()
    }
}