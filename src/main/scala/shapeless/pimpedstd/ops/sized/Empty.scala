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

package shapeless.pimpedstd.ops.sized

import shapeless._
import shapeless.pimpedstd.util.IsTraversableLikeAux

import scala.collection.generic.CanBuildFrom

trait Empty[Repr] extends DepFn0 { type Out = Sized[Repr, _0] }

object Empty {
  
  implicit def empty[Repr, A] (implicit 
    ev: AdditiveCollection[Repr]
  , isTraversableLike: IsTraversableLikeAux[Repr, A]
  , cbf: CanBuildFrom[Nothing, A, Repr]
  ): Empty[Repr] =
    new Empty[Repr] {
      def apply() = Sized.wrap[Repr, _0](cbf().result())
    }
  
}
