use "collections"
use persistent = "collections/persistent"

class MultiSet[A: (Hashable val & Equatable[A] val)]
  let _counts_by_element: Map[A, I64]
  var elements_by_count: persistent.Map[I64, persistent.Vec[A]]

  new ref create(prealloc: USize val = 6) =>
    _counts_by_element = HashMap[A, I64, HashEq[A]](where prealloc = prealloc)
    elements_by_count = persistent.HashMap[I64, persistent.Vec[A], HashEq[I64]]

  fun ref _update(element: A, delta: I64) =>
    if delta != 0 then
      let old_count: I64 = _counts_by_element.get_or_else(element, 0)
      let new_count: I64 = old_count + delta

      _counts_by_element(element) = new_count

      if new_count > 0 then
        let new_count_elements: persistent.Vec[A] =
          elements_by_count.get_or_else(new_count, persistent.Vec[A])
        elements_by_count = elements_by_count(new_count) =
          if delta > 0 then
            new_count_elements.push(element)
          else
            try
              new_count_elements.insert(0, element)?
            else
              // Can't insert when empty
              new_count_elements.push(element)
            end
          end
      end

      if old_count > 0 then
        var old_count_elements: persistent.Vec[A] =
          elements_by_count.get_or_else(old_count, persistent.Vec[A])
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
    end

  fun ref update(increment: A, decrement: (A | None)) =>
    _update(increment, 1)
    match decrement
    | let element': A => _update(element', -1)
    end

  fun ref clear() =>
    _counts_by_element.clear()
    elements_by_count = persistent.HashMap[I64, persistent.Vec[A], HashEq[I64]]
