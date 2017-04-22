package escher

/**
  * Running time related tools
  */
object TimeTools {
  type Nanosecond = Long

  def nanoToMillisString(nanosecond: Nanosecond): String = {
    val millis = (nanosecond /1e9).toInt
    if(millis>0){
      val ms = nanosecond/1e6 - millis * 1000
      "%d,%.2fms".format(millis.toInt, ms)
    } else
      "%.2fms".format(nanosecond /1e6)
  }

  def printTimeUsed[A](taskName: String)(task: => A): A = {
    val (nano, result) = measureTime(task)
    println(s"*** [$taskName] time used: ${nanoToMillisString(nano)} ***")
    result
  }

  def measureTime[A](task: => A): (Nanosecond, A) = {
    val t1 = System.nanoTime()
    val result = task
    val time = System.nanoTime() - t1
    (time, result)
  }

  /** Since this method uses Thread.sleep, it may not be accurate for methods with very short running time */
  def scaleUpRunningTime[A](factor: Int)(task: => A): A = {
    require(factor >= 1)

    if(factor == 1) return task

    val (nano, result) = measureTime(task)
    val extraNano = (factor - 1) * nano
    val millis = extraNano / 1000000
    val nanos = extraNano - millis * 1000000
    Thread.sleep(millis, nanos.toInt)
    result
  }

  @inline
  def runOnce[A](f: => A): A = {
    f
  }

  @inline
  def run5Times[A](f: => A): A = {
    f;f;f;f;f
  }
}
