use "collections"
use persistent = "collections/persistent"

class MultiSet[A: (Hashable val & Equatable[A])]
  let _max_safe_count: U64 = U64.max_value()

  let _counts_by_value: Map[A, U64]
  var values_by_count: persistent.Map[U64, persistent.Vec[A]]

  new ref create(prealloc: USize val = 6) =>
    _counts_by_value = HashMap[A, U64, HashEq[A]](where prealloc = prealloc)
    values_by_count = persistent.HashMap[U64, persistent.Vec[A], HashEq[U64]]

  fun ref _remove_values_by_count(old_count: U64, value: A) =>
    try
      var old_count_values: persistent.Vec[A] = values_by_count(old_count)?
      values_by_count =
        try
          old_count_values = old_count_values.delete(
            old_count_values.find(
              value where predicate = {(l: A, r: A): Bool => l == r}
            )?
          )?

          if old_count_values.size() == 0 then
            values_by_count.remove(old_count)?
          else
            values_by_count(old_count) = old_count_values
          end
        else
          values_by_count
        end
    end

  fun ref _increment(value: A) =>
    let old_count: U64 = _counts_by_value.get_or_else(value, 0)
    if old_count < _max_safe_count then
      let new_count: U64 = old_count + 1

      // Update _counts_by_value
      _counts_by_value(value) = new_count
        
      // Update values_by_count
      let new_count_values: persistent.Vec[A] =
        values_by_count.get_or_else(new_count, persistent.Vec[A])
      values_by_count =
        values_by_count(new_count) = new_count_values.push(value)

      _remove_values_by_count(old_count, value)
    end

  fun ref _decrement(value: A) =>
    try
      let old_count: U64 = _counts_by_value(value)?
      let new_count: U64 = old_count - 1

      if new_count == 0 then
        _counts_by_value.remove(value)?
      else
        // Update _counts_by_value
        _counts_by_value(value) = new_count

        // Update values_by_count
        let new_count_values: persistent.Vec[A] =
          values_by_count.get_or_else(new_count, persistent.Vec[A])
        values_by_count = values_by_count(new_count) =
          try
            new_count_values.insert(0, value)?
          else
            // Can't insert when empty
            new_count_values.push(value)
          end
      end

      _remove_values_by_count(old_count, value)
    end

  fun ref update(increment: A, decrement: (A | None) = None) =>
    _increment(increment)
    match decrement
    | let decrement': A => _decrement(decrement')
    end

  fun ref clear() =>
    _counts_by_value.clear()
    values_by_count = persistent.HashMap[U64, persistent.Vec[A], HashEq[U64]]
