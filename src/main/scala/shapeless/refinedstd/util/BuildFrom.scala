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

package shapeless.refinedstd.util

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
 * Almost an alias for CanBuildFrom-s of the form `CanBuildFrom[From, Elem, To]` with
 * `From` like `CC[_]` and `To` like `CC[Elem]`, except that implicits resolution seems to work better with 
 * this one in some cases.
 * 
 * @tparam From: type of the collection out of which a new one should be built
 * @tparam Elem: type of the elements in the new collection
 */
trait BuildFrom[-From, Elem] {
  type To
  def apply(): mutable.Builder[Elem, To]
}

object BuildFrom {
  type Aux[From, Elem, To0] = BuildFrom[From, Elem] { type To = To0 }

  def apply[From, Elem] (implicit 
    bf: BuildFrom[From, Elem]
  ): Aux[From, Elem, bf.To] = bf

  implicit def buildFrom[CC[_], Elem] (implicit 
    cbf: CanBuildFrom[CC[_], Elem, CC[Elem]]
  ): Aux[CC[_], Elem, CC[Elem]] =
    new BuildFrom[CC[_], Elem] {
      type To = CC[Elem]
      def apply() = cbf()
    }
}