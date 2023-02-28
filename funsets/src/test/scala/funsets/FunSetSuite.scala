package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val sUnion = union(s1,s2)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1,2), "No other elements")
  }

  test("union contains all elements of each set") {
    new TestSets:
      assert(contains(sUnion, 1), "Contains element from s1")
      assert(contains(sUnion, 2), "Contains element from s2")
      assert(!contains(sUnion, 3), "Does not contain element from s3")
  }

  test("intersect contains elements present in both sets") {
    new TestSets:
      val sIntersect = intersect(sUnion,s2)
      assert(contains(sIntersect, 2), "Contains mutual element")
      assert(!contains(sIntersect, 1), "Does not contain element that is only in sUnion")
  }

  test("difference between two sets") {
    new TestSets:
      val sDiff = diff(sUnion, s2)
      assert(contains(sDiff, 1), "Shows diff")
      assert(!contains(sDiff, 2), "Doesn't show a mutual element")
  }

  test("we apply filter") {
    new TestSets:
      assert(contains(filter(sUnion, (x: Int) => x > 1), 2), "Filter works")
      assert(!contains(filter(sUnion, (x: Int) => x > 1), 1), "Negative case for filter")
  }

  test("we test forEach") {
    new TestSets:
      assert(forall((x: Int) => true, (x: Int) => (x >= -1000) && (x <= 1000)), "all elements are within boundaries")
      assert(!forall((x: Int) => true, (x: Int) => x > -1000), "edge test")
      assert(!forall((x: Int) => true, (x: Int) => x < 500), "breach of filter")
      assert(!forall((x: Int) => x % 2 == 0, (x: Int) => x % 2 == 1), "Should break")
      assert(forall((x: Int) => x % 2 == 0, (x: Int) => x % 2 == 0), "Should pass")
  }

  test("we test exists") {
    new TestSets:
      assert(exists((x: Int) => true, (x: Int) => x > 500), "more than is successful")
      assert(exists((x: Int) => true, (x: Int) => x == -1000), "edge case")
      assert(!exists((x: Int) => true, (x: Int) => x > 1500), "breach of filter")
      assert(exists((x: Int) => x % 2 == 0, (x: Int) => x == 4), "Should pass")
      assert(!exists((x: Int) => x % 2 == 0, (x: Int) => x == 3), "Should break")
  }

  test("we test mapping") {
    new TestSets:
      assert(contains(map((x: Int) => x % 3 == 0, (x: Int) => x * x), 9 ), "Pass")
      assert(!contains(map((x: Int) => x % 3 == 0, (x: Int) => x * x), 8 ), "Fail")
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
