use "collections"
use persistent = "collections/persistent"

class MultiSet[A: (Hashable val & Equatable[A])]
  let _max_safe_count: U64 = U64.max_value()

  let _counts_by_element: Map[A, U64]
  var elements_by_count: persistent.Map[U64, persistent.Vec[A]]

  new ref create(prealloc: USize val = 6) =>
    _counts_by_element = HashMap[A, U64, HashEq[A]](where prealloc = prealloc)
    elements_by_count = persistent.HashMap[U64, persistent.Vec[A], HashEq[U64]]

  fun ref _remove_elements_by_count(old_count: U64, element: A) =>
    try
      var old_count_elements: persistent.Vec[A] = elements_by_count(old_count)?
      elements_by_count =
        try
          old_count_elements = old_count_elements.delete(
            old_count_elements.find(
              element where predicate = {(l: A, r: A): Bool => l == r}
            )?
          )?

          if old_count_elements.size() == 0 then
            elements_by_count.remove(old_count)?
          else
            elements_by_count(old_count) = old_count_elements
          end
        else
          elements_by_count
        end
    end

  fun ref _increment(element: A) =>
    let old_count: U64 = _counts_by_element.get_or_else(element, 0)
    if old_count < _max_safe_count then
      let new_count: U64 = old_count + 1

      // Update _counts_by_element
      _counts_by_element(element) = new_count
        
      // Update elements_by_count
      let new_count_elements: persistent.Vec[A] =
        elements_by_count.get_or_else(new_count, persistent.Vec[A])
      elements_by_count =
        elements_by_count(new_count) = new_count_elements.push(element)

      _remove_elements_by_count(old_count, element)
    end

  fun ref _decrement(element: A) =>
    try
      let old_count: U64 = _counts_by_element(element)?
      let new_count: U64 = old_count - 1

      if new_count == 0 then
        _counts_by_element.remove(element)?
      else
        // Update _counts_by_element
        _counts_by_element(element) = new_count

        // Update elements_by_count
        let new_count_elements: persistent.Vec[A] =
          elements_by_count.get_or_else(new_count, persistent.Vec[A])
        elements_by_count = elements_by_count(new_count) =
          try
            new_count_elements.insert(0, element)?
          else
            // Can't insert when empty
            new_count_elements.push(element)
          end
      end

      _remove_elements_by_count(old_count, element)
    end

  fun ref update(increment: A, decrement: (A | None)) =>
    _increment(increment)
    match decrement
    | let decrement': A => _decrement(decrement')
    end

  fun ref clear() =>
    _counts_by_element.clear()
    elements_by_count = persistent.HashMap[U64, persistent.Vec[A], HashEq[U64]]
