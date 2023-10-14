primitive FatalError
  fun _stack_overflow(): Array[USize] =>
    let arr = _stack_overflow()
    try arr.pop()? end
    arr

  fun apply[A](): A =>
    _stack_overflow()
    apply[A]()