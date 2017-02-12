package com.example.catfood

import simulacrum.{ op, typeclass }

package object substitute {

  @typeclass
  trait Semigroup[A] {
    @op("|+|") def append(x: A, y: A): A
  }

}
