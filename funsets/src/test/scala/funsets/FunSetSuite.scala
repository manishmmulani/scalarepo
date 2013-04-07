package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s123 = union(union(s1, s2), s3)
    val s1_to_100: Set = x => x >= 1 && x <= 100
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect function") {
    new TestSets {
      val s1_s2_in = intersect(s1, s2)
      val s2_s123_in = intersect(s2, s123)
      assert(!contains(s1_s2_in, 1), "Intersect {1} and {2} not contain 1")
      assert(!contains(s1_s2_in, 2), "Intersect {1} and {2} not contain 2")

      assert(contains(s2_s123_in, 2), "Intersect {2} and {1,2,3} contain 2")
      assert(!contains(s2_s123_in, 3), "Intersect {2} and {1,2,3} doesn't contain 3")
      assert(!contains(s2_s123_in, 1), "Intersect {2} and {1,2,3} doesn't contain 1")
    }
  }

  test("diff function") {
    new TestSets {
      val s123_diff_s3 = diff(s123, s3)
      assert(contains(s123_diff_s3, 1), "Diff b/w {1,2,3} and {3} contains 1")
      assert(contains(s123_diff_s3, 2), "Diff b/w {1,2,3} and {3} contains 2")
      assert(!contains(s123_diff_s3, 3), "Diff b/w {1,2,3} and {3} doesn't contain 3")
    }
  }

  test("filter function") {
    new TestSets {
      val multiplesOf3 = filter(s1_to_100, x => x % 3 == 0)
      assert(contains(multiplesOf3, 6), "multiplesOf3 contains 6")
      assert(contains(multiplesOf3, 87), "multiplesOf3 contains 87")
      assert(contains(multiplesOf3, 42), "multiplesOf3 contains 42")
      assert(contains(multiplesOf3, 51), "multiplesOf3 contains 51")

      assert(!contains(multiplesOf3, 1), "multiplesOf3 doesn't contain 1")
      assert(!contains(multiplesOf3, 2), "multiplesOf3 doesn't contain 2")
      assert(!contains(multiplesOf3, 4), "multiplesOf3 doesn't contain 4")
      assert(!contains(multiplesOf3, 98), "multiplesOf3 doesn't contain 98")
    }
  }

  test("forall function") {
    val multiplesOf15: Set = x => x % 15 == 0
    assert(forall(multiplesOf15, x => x % 3 == 0), "all multiples of 15 are divisible by 3")
    assert(forall(multiplesOf15, x => x % 5 == 0), "all multiples of 15 are divisible by 5")
    assert(!forall(multiplesOf15, x => x > 0), "all multiples of 15 are not positive")
    assert(!forall(multiplesOf15, x => x % 2 == 0), "all multiples of 15 are not even")
  }

  test("exists function") {
    new TestSets {
      assert(exists(s1_to_100, x => x == 57), "1 to 100 has 57")
      assert(exists(s123, x => x%2 == 0), "{1,2,3} has at least one even number")
      assert(!exists(s1_to_100, x => x > 100), "1 to 100 doesn't have a number greater than 100")
    }
  }
  
  test("map function") {
    new TestSets {
      val mappedSet = map(s123, x => 5*x)
      assert(!contains(mappedSet, 1), "mapped set doesn't contain 1")
      assert(!contains(mappedSet, 2), "mapped set doesn't contain 2")
      assert(!contains(mappedSet, 4), "mapped set doesn't contain 4")

      assert(contains(mappedSet, 5), "mapped set contains 5")
      assert(contains(mappedSet, 10), "mapped set contains 10")
      assert(contains(mappedSet, 15), "mapped set contains 15")
    }
  }
}
