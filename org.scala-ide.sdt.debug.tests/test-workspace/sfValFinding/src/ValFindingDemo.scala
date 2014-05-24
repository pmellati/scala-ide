package valfinding

object Main {
  val zz = 24

  def main(args: Array[String]): Unit = {
    val a = new ScalaClass("nonFieldClassParam", "fieldClassParam")

    val outer = new OuterClass
    val inner = new outer.InnerClass

    val derived = new DerivedClass(47) with TheTrait
    derived.traitFunc()

    (new ExplicitExtenderOfTheTrait).fun

    new EnclosingTrait{}

    Objectt.f("Obj f param")

    (new ClosureTest).useClosures()
  }
}

class ScalaClass(nonFieldClassParam: String, private var fieldClassParam /*{class param & field decl}*/ : String) {
  val classField /*{class field decl}*/ = nonFieldClassParam + fieldClassParam /*{class param & field usage}*/
  classField /*{class field usage}*/

  val cc = CaseC("Case")
  cc.classField /*{field with same name of a field}*/  // Shouldn't find a value.

  func("func param")

  def func(funcParam /*{method param decl}*/: String) = {
    funcParam /*{method param usage}*/
    val localVall /*{method-local variable}*/ = "local val"

    nested1

    def nested1 {
      val nested1Local = "nested1Local"
      nested2("nesteds parameter")

      def nested2(nestedMethodParam /*{nested method param}*/: String) {val nested2Local /*{nested method local}*/ = "nested2Local"
        nested1Local /*{enclosing nested method local}*/
        localVall /*{root enclosing method local}*/
      }
    }
  }

  def someMethodWeWillNeverStepInto(funcParam /*{similarly named param of a method we are not in}*/: String = "similarly named") {
    val localVall /*{similarly named local var of a method we are not in}*/ = "Similarly Named"
  }
}

class ClassIntoWhichWeWillNeverStep(var fieldClassParam /*{similarly named field decl of a class we are not in}*/: String) {
  fieldClassParam /*{similarly named field usage of a class we are not in}*/
}

case class CaseC(classField: String) {
  val f2: String = classField + " field2"
}

class OuterClass {
  val outerExclusiveField = "outerExclusiveField"
  val fff = "Outer's Field"

  class InnerClass {
    val fff /*{inner class field decl}*/ = "Inner's Field"

    testFields

    def testFields {
      val fff = "Local Shadower"

      fff /*{method-local var shadowing field}*/                   // Should show "Local Shadower"
      this.fff /*{shadowed field accessed with this}*/              // Should show "Inner's field"
      InnerClass.this.fff /*{shadowed field accessed with this with class name}*/    // Should show "Inner's field"
      OuterClass.this.fff /*{shadowed field accessed with this with enclosing class name}*/   // Should show "Outer's field"

      outerExclusiveField /*{exclusive field of enclosing class}*/
    }
  }
}

class BaseClass(baseParam: String) {
  val baseField /*{base class field decl}*/ = "baseField"

  def baseFunc(bfParam /*{base class method param}*/: String) {
    baseField /*{base class field usage}*/
  }
}

class DerivedClass(derivedParam: Int) extends BaseClass("baseParamFromDerived") /*with TheTrait*/ {
  val derivedField = "derived field"

  baseFunc("base meth param")
  derivedFunc("dfParam")

  def derivedFunc(dfParam: String) {
    baseField /*{base class field usage from derived class}*/
    derivedField /*{derived class field usage}*/
  }
}

trait TheTrait {
  val traitField /*{trait field decl}*/ = "traitFieldd"

  def traitFunc(tfParam /*{trait method param}*/: String = "traitFuncParam") {
    traitField /*{trait field usage from trait}*/
  }
}

class ExplicitExtenderOfTheTrait extends TheTrait {
  traitField /*{trait field access from ctor of extender}*/

  def fun {
    traitField /*{trait field access from method of extender}*/
    traitFunc()
  }
}

trait EnclosingTrait {
  val traitField = "tField"

  new Nestedd

  class Nestedd {
    val nestedField /*{field decl of class nested in trait}*/ = "nField"
    nestedField /*{field usage of class nested in trait}*/
    traitField /*{field of enclosing trait usage}*/
  }
}

object Objectt {
  val field /*{object field decl}*/ = "obj field"

  def f(param: String) {
    field  /*{object field usage}*/
  }
}

class ClosureTest {
  val fieldC = "captured field"

  def useClosures() {
    val localValC = "Local val captured"
    val shadowedInClosure = "This shouldn't be shown"

    List("clParam1", "clParam2", "clParam3").map {closureParam /*{closure param decl}*/ =>
      closureParam /*{closure param usage}*/
      fieldC /*{captured field of enclosing class}*/
      localValC /*{captured local variable of enclosing method}*/

      val shadowedInClosure /*{local var of closure shadowing local var of enclosing method}*/ = "shadowed in closure"
      shadowedInClosure
    }
  }

  def localSimilar {
    val closureParam /*{local of another method named similarly to a local of closure}*/ = "SSS"
  }
}
