package escher

/**
  * Running time related tools
  */
object TimeTools {
  def printTimeUsed[A](taskName: String)(task: => A): A = {
    val t1 = System.currentTimeMillis()
    val result = task
    val time = System.currentTimeMillis() - t1
    println(s"*** [$taskName] time used: $taskName: ${time}ms ***")
    result
  }
}
