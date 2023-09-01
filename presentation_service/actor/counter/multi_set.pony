use "collections"
use persistent = "collections/persistent"

class MultiSet[A: (Hashable val & Equatable[A] val)]
  let _counts_by_item: Map[A, I64]
  var items_by_count: persistent.Map[I64, persistent.Vec[A]]

  new ref create(prealloc: USize val = 6) =>
    _counts_by_item = HashMap[A, I64, HashEq[A]](where prealloc = prealloc)
    items_by_count = persistent.HashMap[I64, persistent.Vec[A], HashEq[I64]]

  fun ref _update(item: A, delta: I64) =>
    if delta != 0 then
      let old_count: I64 = _counts_by_item.get_or_else(item, 0)
      let new_count: I64 = old_count + delta

      _counts_by_item(item) = new_count

      if new_count > 0 then
        let new_count_items: persistent.Vec[A] =
          items_by_count.get_or_else(new_count, persistent.Vec[A])
        items_by_count = items_by_count(new_count) =
          if delta > 0 then
            new_count_items.push(item)
          else
            try
              new_count_items.insert(0, item)?
            else
              // Can't insert when empty
              new_count_items.push(item)
            end
          end
      end

      if old_count > 0 then
        var old_count_items: persistent.Vec[A] =
          items_by_count.get_or_else(old_count, persistent.Vec[A])
        items_by_count =
          try
            old_count_items = old_count_items.delete(
              old_count_items.find(
                item where predicate = {(l: A, r: A): Bool => l == r}
              )?
            )?

            if old_count_items.size() == 0 then
              items_by_count.remove(old_count)?
            else
              items_by_count(old_count) = old_count_items
            end
          else
            items_by_count
          end
      end
    end

  fun ref update(increment: A, decrement: (A | None)) =>
    _update(increment, 1)
    match decrement
    | let item': A => _update(item', -1)
    end

  fun ref clear() =>
    _counts_by_item.clear()
    items_by_count = persistent.HashMap[I64, persistent.Vec[A], HashEq[I64]]
