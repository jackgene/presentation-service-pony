use "collections"
use persistent = "collections/persistent"
use "itertools"

type Effect[A: (Hashable val & Equatable[A])] is (Pushed[A] | PushedEvicting[A])

class val Pushed[A: (Hashable val & Equatable[A])]
  let value: A

  new val create(value': A) =>
    value = value'

class val PushedEvicting[A: (Hashable val & Equatable[A])]
  let value: A
  let evicting: A

  new val create(value': A, evicting': A) =>
    value = value'
    evicting = evicting'

class FIFOBoundedSet[A: (Hashable val & Equatable[A])]
  let _max: USize
  let _uniques: Set[A]
  var insertion_order: persistent.Vec[A]

  new create(max: USize val = 3) =>
    _max = max
    _uniques = HashSet[A, HashEq[A]](where prealloc = max)
    insertion_order = persistent.Vec[A]

  fun ref push(value: A): (Effect[A] val | None) =>
    if _uniques.contains(value) then
      try
        if insertion_order(insertion_order.size() - 1)? != value then
          // Move to end
          insertion_order = insertion_order
            .delete(
              insertion_order.find(
                value where predicate = {(l: A, r: A): Bool => l == r}
              )?
            )?
            .push(value)
        end
      end

      None
    else
      _uniques.set(value)
      insertion_order = insertion_order.push(value)

      if _uniques.size() <= _max then
        Pushed[A](value)
      else
        try
          let oldestValue: A = insertion_order(0)?
          insertion_order = insertion_order.delete(0)?
          _uniques.unset(oldestValue)

          PushedEvicting[A](value, oldestValue)
        else
          // This really should never happen
          Pushed[A](value)
        end
      end
    end

  fun ref union(that: Iterator[A] ref): Array[Effect[A] val] iso^ =>
    let old_uniques: Set[A] = HashSet[A, HashEq[A]](
      where prealloc = _uniques.size()
    )
    old_uniques.union(_uniques.values())
    let old_insertion_order: Array[A] = Array[A]
    old_insertion_order.concat(insertion_order.values())

    let pushes: Array[A] = Array[A]
    for value in that do
      match push(value)
      | let effect: Pushed[A] => pushes.push(effect.value)
      | let effect: PushedEvicting[A] => pushes.push(effect.value)
      end
    end

    let effective_evict_set: Set[A] = HashSet[A, HashEq[A]](
      where prealloc = old_uniques.size()
    )
    effective_evict_set.union(old_uniques.values())
    effective_evict_set.difference(_uniques.values())

    let effective_evicts: Array[A] = Array[A]
    for value in old_insertion_order.values() do
      if effective_evict_set.contains(value) then
        effective_evicts.push(value)
      end
    end
    effective_evicts.reverse_in_place()

    let effective_push_set: Set[A] = HashSet[A, HashEq[A]](
      where prealloc = _uniques.size()
    )
    effective_push_set.union(_uniques.values())
    effective_push_set.difference(old_uniques.values())

    let effective_pushes: Array[A] = Array[A]
    for value in pushes.values() do
      if effective_push_set.contains(value) then
        effective_pushes.push(value)
      end
    end
    effective_pushes.reverse_in_place()

    let effects: Array[Effect[A]] iso = Array[Effect[A]]
    let effective_pushes_values: Iterator[A] = effective_pushes.values()
    for 
      (value, evicting) in Iter[A](effective_pushes_values)
        .zip[A](effective_evicts.values())
    do
      effects.push(PushedEvicting[A](value, evicting))
    end
    // There can be more pushes to values (when FIFOBoundedSet isn't full yet)
    for value in effective_pushes_values do
      effects.push(Pushed[A](value))
    end
    effects.reverse_in_place()

    consume effects
