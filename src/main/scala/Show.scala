trait Show[A]:
  def show(a: A): String

object Show:
  extension [A: Show](a: A)
    def show: String = summon[Show[A]].show(a)
