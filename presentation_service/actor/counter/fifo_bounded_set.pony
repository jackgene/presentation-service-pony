use "collections"
use persistent = "collections/persistent"

type Effect is (Pushed | PushedEvicting)

class val Pushed
  let value: String

  new val create(value': String) =>
    value = value'

class val PushedEvicting
  let value: String
  let evicting: String

  new val create(value': String, evicting': String) =>
    value = value'
    evicting = evicting'

class FIFOBoundedSet[A: String val]
  let _max: USize
  let _uniques: Set[A]
  var insertion_order: persistent.Vec[A]

  new ref create(max: USize val = 3) =>
    _max = max
    _uniques = HashSet[A, HashEq[A]](where prealloc = max)
    insertion_order = persistent.Vec[A]

  fun ref push(value: A): (Effect val | None) =>
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
        Pushed(value)
      else
        try
          let oldestValue: A = insertion_order(0)?
          insertion_order = insertion_order.remove(0, 1)?
          _uniques.unset(oldestValue)

          PushedEvicting(value, oldestValue)
        else
          // This really should never happen
          Pushed(value)
        end
      end
    end

  fun ref union(that: Iterator[A] ref): Array[Effect val] iso^ =>
    let old_uniques: Set[A] = HashSet[A, HashEq[A]](where prealloc = _max)
    old_uniques.union(_uniques.values())

    let effects: Array[Effect] iso = Array[Effect]
    for value in that do
      match push(value)
      | let effect: PushedEvicting if not old_uniques.contains(effect.evicting) =>
        effects.push(Pushed(effect.value))
      | let effect: Effect =>
        effects.push(effect)
      end
    end

    if effects.size() > _max then
      effects.trim_in_place(effects.size() - _max)
    end

    consume effects
