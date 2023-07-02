abstract class ParametrizedSuite extends munit.FunSuite:
  import ParametrizedSuite.*

  def parametrizedTest[A](
      label: String,
      params: List[TestParam[A]]
  )(f: A => Unit) =
    params.foreach: (param) =>
      val testName =
        s"""$label ${param.label.map(l => s"[$l]").getOrElse("")}"""
      test(testName)(f(param.input))

object ParametrizedSuite:
  class TestParam[A](val label: Option[String], val input: A)
  object TestParam:
    def apply[A](input: A): TestParam[A] = new TestParam(None, input)
    def apply[A](label: String, input: A): TestParam[A] =
      new TestParam(Some(label), input)
