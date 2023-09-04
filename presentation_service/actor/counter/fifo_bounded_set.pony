use "collections"
use persistent = "collections/persistent"

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

  new ref create(max: USize val = 3) =>
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
    let old_uniques: Set[A] = HashSet[A, HashEq[A]](where prealloc = _max)
    old_uniques.union(_uniques.values())

    let effects: Array[Effect[A]] iso = Array[Effect[A]]
    for value in that do
      match push(value)
      | let effect: PushedEvicting[A] if not old_uniques.contains(effect.evicting) =>
        effects.push(Pushed[A](effect.value))
      | let effect: Effect[A] =>
        effects.push(effect)
      end
    end

    if effects.size() > _max then
      effects.trim_in_place(effects.size() - _max)
    end

    consume effects
