## Entity Trait System

[![Crates.io](https://img.shields.io/crates/v/entity-trait-system.svg)](https://crates.io/crates/entity-trait-system)



Entity trait system (ETS) is an alternative to entity component system
architectures.

Here is a [video](https://youtu.be/AezHJdwDfW0) summary.

Optional features:

 - [serde](https://crates.io/crates/serde) support
 - [rayon](https://crates.io/crates/rayon) support

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
// compile time type accessor of arena member
world.arena_mut::<Enemy>().insert(Enemy { hp: 10 });

// visit all arena with types that implement trait - bound at compile time
#[cfg(feature = "rayon")]
world.par_visit_test_trait(|e| e.do_something());
#[cfg(not(feature = "rayon"))]
world.visit_test_trait(|e| e.do_something());

// runtime type API - access object without knowing the type beforehand
let arena_id = MyWorld::arena_id::<Player>();
let arena = world.arena_erased(arena_id);
// unwrap: I know that this is a player and that the reference is valid
let player = arena
    .get_any(player_id)
    .unwrap()
    .downcast_ref::<Player>()
    .unwrap();
player.do_something();
```