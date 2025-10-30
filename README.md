## Entity Trait System

[![Crates.io](https://img.shields.io/crates/v/entity-trait-system.svg)](https://crates.io/crates/entity-trait-system)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Entity trait system (ETS) is an alternative to entity component system. Here is
a [video](https://youtu.be/AezHJdwDfW0) summary.

Requires the nightly compiler and
[specialization](https://std-dev-guide.rust-lang.org/policy/specialization.html)
feature - it's only used in a sound way.

## Example

```rs
// explicitly opt in to language feature
#![allow(incomplete_features)]
#![feature(specialization)]

// declare world, entities, and traits which the entities could have
entity_trait_system::world!(MyWorld, Enemy, Player; TestTrait, SecondTestTrait);

let mut world = MyWorld::default();
// directly access arena member
let player_id = world.player.insert(Player { id: 1 });
// compile time type accessor of arena member (similar)
world.arena_mut::<Enemy>().insert(Enemy { hp: 10 });

// visit all arenas with types that implement trait - bound at compile time
#[cfg(feature = "rayon")]
world.par_visit_test_trait(|e| e.do_something());
#[cfg(not(feature = "rayon"))]
world.visit_test_trait(|e| e.do_something());

// runtime type API - access type-erased (Any) arena
let arena_id = MyWorld::arena_id::<Player>();
let player_arena = world.any_arena_mut(arena_id);
// unwrap: I know that this is a player and that the reference is valid
let player = player_arena
    .get_mut(player_id).unwrap()
    .downcast_mut::<Player>().unwrap();
player.do_something_else();
```

## API Overview

### Per-Trait Methods (generated for each trait)
- `visit_<trait>` - Immutable iteration over implementing entities
- `visit_mut_<trait>` - Mutable iteration over implementing entities
- `visit_key_<trait>` - Immutable iteration with `(Key, &Value)` tuples
- `visit_key_mut_<trait>` - Mutable iteration with `(Key, &mut Value)` tuples
- `retain_<trait>` - Keep entities matching predicate
- `retain_with_default_<trait>` - Keep with control over non-implementing types
- `diff_<trait>` - Gather diff vector from immutable view, to apply later.
- `diff_mut_<trait>` - Same as previous, but from mutable view.
- `diff_apply_<trait>` - Apply diff vector
- `clear_<trait>` - Clear all arenas implementing trait
- `len_<trait>` - Count entities implementing trait
- `any_arenas_<trait>` - Get fixed-size array of type-erased arenas implementing trait
- `any_arenas_mut_<trait>` - Mutable version of above

### World Methods
- `arena<T>()` / `arena_mut<T>()` - Compile-time typed arena access
- `any_arena()` / `any_arena_mut()` - Runtime type-erased access
- `arena_id<T>()` - Get stable arena identifier - serializes to type name.
- `clear()` - Clear all arenas
- `len()` - Total entity count across all arenas

## Performance Notes

- **Zero-cost abstractions**: Trait checks are compile-time via specialization
- **Cache-friendly**: Uses `slotmap::DenseSlotMap` for contiguous storage per type
- **Parallel safety**: `par_*` methods leverage rayon for multi-threaded iteration
- **No virtual dispatch overhead** unless explicitly using type-erased api

## Rayon Support
Parallel via `par_*` variants.

## Serde Support
Both map (serde_json) and seq (bincode) style serialization.
