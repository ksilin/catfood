package com.example.catfood.existentials

import org.scalatest.{ FreeSpec, MustMatchers }

class ExistentialPrerequisitesSpec extends FreeSpec with MustMatchers {

  "must work a as a type erasure" in {

    trait Existential {
      type Inner
      val value: Inner
    }

    case class ExOne(value: Int) extends Existential {
      override type Inner = Int
    }

    val ex1: Existential = ExOne(1)
    println(ex1.getClass)

    final case class TypeErasor[A](value: A) extends Existential { type Inner = A }

    val intErased: Existential = TypeErasor(1)
    println(intErased)
    val v1: intErased.Inner = intErased.value
    val stringErased        = TypeErasor("abc")
    println(stringErased)
    val v2: stringErased.Inner = stringErased.value
    val caseErased             = TypeErasor(ExOne(4))
    println(caseErased)
    val v3: caseErased.Inner = caseErased.value

    // we lost the information about the type of the .value of each Existential instance.
    //  but we can still add restrictions to it

  }

}
