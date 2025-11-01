/*!
[![Crates.io](https://img.shields.io/crates/v/entity-trait-system.svg)](https://crates.io/crates/entity-trait-system)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

An alternative to [ECS](https://en.wikipedia.org/wiki/Entity_component_system).
Here is a [video](https://youtu.be/AezHJdwDfW0) summary.

# Setup

Requires the nightly compiler and
[specialization](https://std-dev-guide.rust-lang.org/policy/specialization.html)
feature - it's only used in a sound way.

```bash
rustup override set nightly # enable nightly compiler
cargo add entity-trait-system # get lib
```

```rs
// explicitly opt in to language feature
#![allow(incomplete_features)]
#![feature(specialization)]

// declare world, entities, and traits which the entities could have
entity_trait_system::world!(
    MyWorld, Enemy, Player; TestTrait, SecondTestTrait);

let mut world = MyWorld::default();
// directly access arena member
let player_id = world.player.insert(Player { id: 1 });
// compile time type accessor of arena member (similar)
world.arena_mut::<Enemy>().insert(Enemy { hp: 10 });

// visit all arenas with types that implement trait
// (likely static dispatch)
#[cfg(feature = "rayon")]
world.par_visit_test_trait(|e| e.do_something());
#[cfg(not(feature = "rayon"))]
world.visit_test_trait(|e| e.do_something());

// runtime type API - access type-erased (Any) arena
let arena_id = MyWorld::arena_id::<Player>();
let player_arena = world.any_arena_mut(arena_id);
// unwrap: I know that this is a player
// and that the reference is valid
let player = player_arena
    .get_mut(player_id).unwrap()
    .downcast_mut::<Player>().unwrap();
player.do_something_else();
```

# API Overview

## Per-Trait Methods (generated for each trait)
- `visit_<trait>` - Iter over implementing entities
- `visit_mut_<trait>` - Mutable iter over implementing entities
- `visit_key_<trait>` - Iter `(Key, &Value)` tuples
- `visit_key_mut_<trait>` - Mutable iter `(Key, &mut Value)` tuples
- `retain_<trait>` - Keep entities matching predicate
- `diff_<trait>` - Gather diff vector from immutable view, to apply later
- `diff_mut_<trait>` - Same as previous, but from mutable view
- `diff_apply_<trait>` - Apply diff vector
- `clear_<trait>` - Clear all arenas implementing trait
- `len_<trait>` - Count entities implementing trait
- `any_arenas_<trait>` - Array of type-erased arenas implementing trait
- `any_arenas_mut_<trait>` - Mutable version of above

## World Methods
- `arena<T>()` / `arena_mut<T>()` - Compile-time typed arena access
- `any_arena()` / `any_arena_mut()` - Runtime type-erased access
- `arena_id<T>()` - Get stable arena identifier - serializes to type name.
- `clear()` - Clear all arenas
- `len()` - Total entity count across all arenas

# Rayon Support
Parallel operations exposed via `par_*` variants.

# Serde Support
Both map (serde_json) and seq (bincode) style ser/deserialization.

# Performance Note

## Benchmarks

Here's a [benchmark test suite](https://github.com/jagprog5/ecs_bench_suite)
comparing ETS to other popular libraries.

### Simple Insert

ETS performed the best. It's the same speed as the underlying dense slot map
insertion.

### Simple Iter

The results for this benchmark form two distinct groups. The fastest group,
including hecs and legion, arrange the components of the data as a structure of
arrays. The second group (including ETS) iterate through an array of structures.
A structure of arrays performs better since only the relevant data is loaded
into the cache.

Since ETS doesn't use components, it is inside the second group.

### Fragmented Iter

ETS arrived in second place, just behind shipyard.

### System Scheduling

ETS performed the best. But, disjoint systems (outer parallelism) must be stated
explicitly.

### Heavy Compute

ETS median performance was the best, but there was a higher variance with other
libraries which sometimes is better.

### Add/Remove Component

ETS is omitted from this benchmark since it doesn't use components; it's not
applicable. For sparse components, an auxiliary structure can store entity keys.
For dense components, a trait in the ETS can implement the desire behaviour.

### Serialize

Only three libraries implemented serialization. ETS arrived in second.

## Optimization

This can be found in the implementation of `visit_*`:

```ignore
fn v_if_applicable<F>(
    arena: &DenseSlotMap<DefaultKey, T>,
    mut handler: F) where F: FnMut(&dyn $trait_name)
{
    arena.values_as_slice().iter()
        .for_each(|entity| {
            // implicit type erase T -> &dyn $trait_name
            handler(entity) 
        }); 
}
```

The handler is typically inlined and devirtualized to erase dynamic dispatch,
since the type is known at compile time and is type erased just before use. This
means that static dispatch is reliant on compiler optimization; likely but not
guaranteed.
*/

// specialization feature is incomplete, but is being used here in a very
// limited but required capacity. it is only used to check if a type implements
// a trait - there is no overlapping impl weirdness that is relied on
#![allow(incomplete_features)]
#![feature(specialization)]

// macro is expanded at call site - makes it available to the lib user
//
// below macro references these via $crate:: prefix
#[doc(hidden)]
pub use paste;
#[cfg(feature = "rayon")]
#[doc(hidden)]
pub use rayon;
#[cfg(feature = "serde")]
#[doc(hidden)]
pub use serde;
#[doc(hidden)]
pub use slotmap;

#[doc(hidden)]
pub trait IsType<T> {
    const VALUE: bool = false;
}

impl<A, B> IsType<B> for A {
    default const VALUE: bool = false;
}

// specialize for equal types
impl<T> IsType<T> for T {
    const VALUE: bool = true;
}

/// type erased view of an arena. exposes single element API from DenseSlotMap
#[doc(hidden)]
pub trait ErasedArena {
    fn get(&self, key: slotmap::DefaultKey) -> Option<&dyn std::any::Any>;
    fn get_mut(&mut self, key: slotmap::DefaultKey) -> Option<&mut dyn std::any::Any>;
    unsafe fn get_unchecked(&self, key: slotmap::DefaultKey) -> &dyn std::any::Any;
    unsafe fn get_unchecked_mut(&mut self, key: slotmap::DefaultKey) -> &mut dyn std::any::Any;
    fn insert(&mut self, value: Box<dyn std::any::Any>) -> slotmap::DefaultKey;
    fn insert_with_key(
        &mut self,
        f: Box<dyn FnOnce(slotmap::DefaultKey) -> Box<dyn std::any::Any>>,
    ) -> slotmap::DefaultKey;
    fn remove(&mut self, key: slotmap::DefaultKey) -> Option<Box<dyn std::any::Any>>;
}

impl<T: 'static> ErasedArena for slotmap::DenseSlotMap<slotmap::DefaultKey, T> {
    fn get(&self, key: slotmap::DefaultKey) -> Option<&dyn std::any::Any> {
        self.get(key).map(|e| e as &dyn std::any::Any)
    }

    fn get_mut(&mut self, key: slotmap::DefaultKey) -> Option<&mut dyn std::any::Any> {
        self.get_mut(key).map(|e| e as &mut dyn std::any::Any)
    }

    unsafe fn get_unchecked(&self, key: slotmap::DefaultKey) -> &dyn std::any::Any {
        unsafe {
            <slotmap::DenseSlotMap<slotmap::DefaultKey, T>>::get_unchecked(self, key)
                as &dyn std::any::Any
        }
    }

    unsafe fn get_unchecked_mut(&mut self, key: slotmap::DefaultKey) -> &mut dyn std::any::Any {
        unsafe {
            <slotmap::DenseSlotMap<slotmap::DefaultKey, T>>::get_unchecked_mut(self, key)
                as &mut dyn std::any::Any
        }
    }

    fn insert(&mut self, value: Box<dyn std::any::Any>) -> slotmap::DefaultKey {
        // expect never triggers, gated by caller
        self.insert(*value.downcast::<T>().expect("Try mismatch in insert"))
    }

    fn insert_with_key(
        &mut self,
        f: Box<dyn FnOnce(slotmap::DefaultKey) -> Box<dyn std::any::Any>>,
    ) -> slotmap::DefaultKey {
        // expect never triggers, gated by caller
        self.insert_with_key(|k| {
            *f(k)
                .downcast::<T>()
                .expect("Type mismatch in insert_with_key")
        })
    }

    fn remove(&mut self, key: slotmap::DefaultKey) -> Option<Box<dyn std::any::Any>> {
        self.remove(key)
            .map(|v| Box::new(v) as Box<dyn std::any::Any>)
    }
}

#[doc(hidden)]
pub trait ArenaCast<T> {
    fn cast(&self) -> &slotmap::DenseSlotMap<slotmap::DefaultKey, T>;
    fn cast_mut(&mut self) -> &mut slotmap::DenseSlotMap<slotmap::DefaultKey, T>;
}

// default: panic for mismatched types
impl<A, B> ArenaCast<B> for slotmap::DenseSlotMap<slotmap::DefaultKey, A> {
    default fn cast(&self) -> &slotmap::DenseSlotMap<slotmap::DefaultKey, B> {
        panic!(
            // never, gated at compile time
            "Arena type mismatch: {} cannot be cast to {}",
            std::any::type_name::<A>(),
            std::any::type_name::<B>()
        );
    }

    default fn cast_mut(&mut self) -> &mut slotmap::DenseSlotMap<slotmap::DefaultKey, B> {
        panic!(
            // never, gated at compile time
            "Arena type mismatch: {} cannot be cast to {}",
            std::any::type_name::<A>(),
            std::any::type_name::<B>()
        );
    }
}

// specialization: types match
impl<T> ArenaCast<T> for slotmap::DenseSlotMap<slotmap::DefaultKey, T> {
    fn cast(&self) -> &slotmap::DenseSlotMap<slotmap::DefaultKey, T> {
        self
    }

    fn cast_mut(&mut self) -> &mut slotmap::DenseSlotMap<slotmap::DefaultKey, T> {
        self
    }
}

// conditional serde if specific type is also serde
#[cfg(feature = "serde")]
#[doc(hidden)] // must be exposed to caller from macro expansion
pub trait SerdeArena<'de> {
    // member serialize into a SerializeStruct (called from world Serialize impl)
    fn serialize_arena<S>(&self, field_name: &'static str, state: &mut S) -> Result<(), S::Error>
    where
        S: serde::ser::SerializeStruct;

    // member deserialize from map access (JSON, etc.)
    fn deserialize_arena<M>(map: &mut M) -> Result<Self, M::Error>
    where
        M: serde::de::MapAccess<'de>,
        Self: Sized;

    // sequence deserialize (e.g. bincode)
    fn from_seq<V>(seq: &mut V, field_name: &str) -> Result<Self, V::Error>
    where
        V: serde::de::SeqAccess<'de>,
        Self: Sized;

    const ACTIVE: bool; // whether this arena participates in serde
}

// default: type does NOT implement serde => do nothing
#[cfg(feature = "serde")]
impl<'de, T> SerdeArena<'de> for slotmap::DenseSlotMap<slotmap::DefaultKey, T> {
    default fn serialize_arena<S>(
        &self,
        _field_name: &'static str,
        _state: &mut S,
    ) -> Result<(), S::Error>
    where
        S: serde::ser::SerializeStruct,
    {
        Ok(())
    }

    default fn deserialize_arena<M>(_map: &mut M) -> Result<Self, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        Ok(slotmap::DenseSlotMap::new())
    }

    default fn from_seq<V>(_seq: &mut V, _field_name: &str) -> Result<Self, V::Error>
    where
        V: serde::de::SeqAccess<'de>,
    {
        Ok(slotmap::DenseSlotMap::new())
    }

    default const ACTIVE: bool = false;
}

// specialized: type implements serde Serialize + Deserialize
#[cfg(feature = "serde")]
impl<'de, T> SerdeArena<'de> for slotmap::DenseSlotMap<slotmap::DefaultKey, T>
where
    T: serde::Serialize + serde::Deserialize<'de>,
{
    fn serialize_arena<S>(&self, field_name: &'static str, state: &mut S) -> Result<(), S::Error>
    where
        S: serde::ser::SerializeStruct,
    {
        state.serialize_field(field_name, self)
    }

    fn deserialize_arena<M>(map: &mut M) -> Result<Self, M::Error>
    where
        M: serde::de::MapAccess<'de>,
    {
        map.next_value()
    }

    fn from_seq<V>(seq: &mut V, field_name: &str) -> Result<Self, V::Error>
    where
        V: serde::de::SeqAccess<'de>,
    {
        seq.next_element()?
            .ok_or_else(|| serde::de::Error::custom(format!("Missing element for {}", field_name)))
    }

    const ACTIVE: bool = true;
}

#[doc(hidden)]
#[macro_export]
#[cfg(not(feature = "rayon"))]
macro_rules! __world_define_rayon_trait_helpers {
    ($struct_name:ident $( $trait_name:ident ),*) => {};
}

#[doc(hidden)]
#[macro_export]
#[cfg(feature = "rayon")]
macro_rules! __world_define_rayon_trait_helpers {
    ($struct_name:ident $( $trait_name:ident ),*) => {
        $crate::paste::paste! {

$(
trait [<$struct_name ParVisitIf $trait_name>]<T> {
    fn pv_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(&dyn $trait_name) + Send + Sync;

    fn pvm_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(&mut dyn $trait_name) + Send + Sync;

    fn pvk_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name) + Send + Sync;

    fn pvkm_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name) + Send + Sync;

    fn pr_if_applicable<P>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, predicate: &P)
    where
        P: Fn(&mut dyn $trait_name) -> bool + Send + Sync;

    fn pd_if_applicable<D, F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F, out: &mut [std::mem::MaybeUninit<D>])
    where
        D: Send,
        F: Fn(&dyn $trait_name) -> D + Send + Sync;

    fn pdm_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F, out: &mut [std::mem::MaybeUninit<D>])
    where
        D: Send,
        F: Fn(&mut dyn $trait_name) -> D + Send + Sync;

    // parallel diff apply
    fn pda_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F, i: &[D])
    where
        D: Sync,
        F: Fn(&mut dyn $trait_name, &D) + Send + Sync;

    const ACTIVE: bool;
}

impl<T> [<$struct_name ParVisitIf $trait_name>]<T> for () {
    default fn pv_if_applicable<F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(&dyn $trait_name) + Send + Sync {}

    default fn pvm_if_applicable<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(&mut dyn $trait_name) + Send + Sync {}

    default fn pvk_if_applicable<F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name) + Send + Sync {}

    default fn pvkm_if_applicable<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name) + Send + Sync {}

    default fn pr_if_applicable<P>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _predicate: &P)
    where P: Fn(&mut dyn $trait_name) -> bool + Send + Sync {}

    default fn pd_if_applicable<D, F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F, _out: &mut [std::mem::MaybeUninit<D>])
    where
        D: Send,
        F: Fn(&dyn $trait_name) -> D + Send + Sync {}

    default fn pdm_if_applicable<D, F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F, _out: &mut [std::mem::MaybeUninit<D>])
    where
        D: Send,
        F: Fn(&mut dyn $trait_name) -> D + Send + Sync {}

    default fn pda_if_applicable<D, F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F, _i: &[D])
    where
        D: Sync,
        F: Fn(&mut dyn $trait_name, &D) + Send + Sync {}

    default const ACTIVE: bool = false;
}

impl<T> [<$struct_name ParVisitIf $trait_name>]<T> for ()
where
    T: $trait_name + Send + Sync,
{
    fn pv_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(&dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_slice()
            .par_iter()
            .for_each(|entity| handler(entity));
    }

    fn pvm_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(&mut dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefMutIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_mut_slice()
            .par_iter_mut()
            .for_each(|entity| handler(entity));
    }

    fn pvk_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefIterator;
        use $crate::rayon::iter::IndexedParallelIterator;
        use $crate::rayon::iter::ParallelIterator;
        let keys = arena.keys_as_slice();
        let values = arena.values_as_slice();
        keys.par_iter()
            .zip(values.par_iter())
            .for_each(|(k, v)| handler(($struct_name::arena_id::<T>(), *k), v));
    }

    fn pvkm_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefIterator;
        use $crate::rayon::iter::IntoParallelRefMutIterator;
        use $crate::rayon::iter::IndexedParallelIterator;
        use $crate::rayon::iter::ParallelIterator;
        let (keys, values) = arena.keys_values_as_mut_slices();
        keys.par_iter()
            .zip(values.par_iter_mut())
            .for_each(|(k, v)| handler(($struct_name::arena_id::<T>(), *k), v));
    }

    fn pr_if_applicable<P>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, predicate: &P)
    where P: Fn(&mut dyn $trait_name) -> bool + Send + Sync
    {
        // current par retain is only parallel between arenas, not per element.
        //
        // I don't think it's possible to do this better but I might be wrong.
        // keeping send + sync for forward compat. either way, not supported by
        // DenseSlotMap
        arena.retain(|_, entity| predicate(entity));
    }

    fn pd_if_applicable<D, F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F, out: &mut [std::mem::MaybeUninit<D>])
    where
        D: Send,
        F: Fn(&dyn $trait_name) -> D + Sync + Send,
    {
        use $crate::rayon::iter::IntoParallelRefMutIterator;
        use $crate::rayon::iter::IndexedParallelIterator;
        use $crate::rayon::iter::IntoParallelRefIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_slice()
            .par_iter()
            .zip(out.par_iter_mut())
            .for_each(|(e, out_slot)| {
                *out_slot = std::mem::MaybeUninit::new(handler(e));
            });
    }

    fn pdm_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F, out: &mut [std::mem::MaybeUninit<D>])
    where
        D: Send,
        F: Fn(&mut dyn $trait_name) -> D + Sync + Send,
    {
        use $crate::rayon::iter::IntoParallelRefMutIterator;
        use $crate::rayon::iter::IndexedParallelIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_mut_slice()
            .par_iter_mut()
            .zip(out.par_iter_mut())
            .for_each(|(e, out_slot)| {
                *out_slot = std::mem::MaybeUninit::new(handler(e));
            });
    }

    fn pda_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F, i: &[D])
    where
        D: Sync,
        F: Fn(&mut dyn $trait_name, &D) + Send + Sync {
            use $crate::rayon::iter::IntoParallelRefMutIterator;
            use $crate::rayon::iter::IndexedParallelIterator;
            use $crate::rayon::iter::IntoParallelRefIterator;
            use $crate::rayon::iter::ParallelIterator;
            arena
                .values_as_mut_slice()
                .par_iter_mut()
                .zip(i.par_iter())
                .for_each(|(e, in_value)| {
                    handler(e, in_value);
                });
        }

    const ACTIVE: bool = true;
}

)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __world_define_visitors_common {
    // https://stackoverflow.com/a/37754096/15534181
    //
    // generate visit_* functions per trait (non-parallel)
    (@pass_entity_tuple $struct_name:ident $($trait_name:ident),* @ $entity_tuple:tt) => {
        $crate::paste::paste! {
            $(
                /// visit all entities that implement the trait
                #[allow(unused)]
                pub fn [<visit_ $trait_name:snake>]<F>(&self, mut handler: F)
                where
                    F: FnMut(&dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@v_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// mutably visit all entities that implement the trait
                #[allow(unused)]
                pub fn [<visit_mut_ $trait_name:snake>]<F>(&mut self, mut handler: F)
                where
                    F: FnMut(&mut dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@vm_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// visit all entities that implement the trait, with their keys
                /// 
                /// since the slotmap key is type agnostic, it's important to
                /// keep the arena id together with the slot map key (don't use
                /// the slot map key on the wrong slot map)
                #[allow(unused)]
                pub fn [<visit_key_ $trait_name:snake>]<F>(&self, mut handler: F)
                where
                    F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@vk_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// mutably visit all entities that implement the trait, with their keys
                /// 
                /// since the slotmap key is type agnostic, it's important to
                /// keep the arena id together with the slot map key (don't use
                /// the slot map key on the wrong slot map)
                #[allow(unused)]
                pub fn [<visit_key_mut_ $trait_name:snake>]<F>(&mut self, mut handler: F)
                where
                    F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@vkm_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// forwards to retain_with_default with DEFAULT=true
                #[allow(unused)]
                pub fn [<retain_ $trait_name:snake>]<F>(&mut self, mut predicate: F)
                where
                    F: FnMut(&mut dyn $trait_name) -> bool
                {
                    self.[<retain_with_default_ $trait_name:snake>]::<true, F>(predicate)
                }

                /// retain entities matching predicate, removing those that
                /// don't.
                ///
                /// predicate applies only to entities implementing the trait.
                ///
                /// others instead use the DEFAULT value - if true entities not
                /// implementing trait are kept, false they are removed.
                #[allow(unused)]
                pub fn [<retain_with_default_ $trait_name:snake>]<const DEFAULT: bool, F>(&mut self, mut predicate: F)
                where
                    F: FnMut(&mut dyn $trait_name) -> bool
                {
                    $crate::__world_define_visitors_common!(@r_use_entity_tuple $struct_name $trait_name $entity_tuple self predicate DEFAULT);
                }

                /// produce a diff vector, created from viewing all elements
                /// that implement trait
                ///
                /// entities must not be inserted or removed between diff
                /// creation and application
                #[allow(unused)]
                pub fn [<diff_ $trait_name:snake>]<D, F>(&mut self, mut handler: F) -> Vec<D>
                where
                    F: FnMut(&dyn $trait_name) -> D
                {
                    $crate::__world_define_visitors_common!(@d_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// produce a diff vector, created from mutably viewing all
                /// elements that implement trait
                ///
                /// entities must not be inserted or removed between diff
                /// creation and application
                #[allow(unused)]
                pub fn [<diff_mut_ $trait_name:snake>]<D, F>(&mut self, mut handler: F) -> Vec<D>
                where
                    F: FnMut(&mut dyn $trait_name) -> D
                {
                    $crate::__world_define_visitors_common!(@dm_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// apply the diff vector.
                ///
                /// entities must not be inserted or removed between diff
                /// creation and application
                #[allow(unused)]
                pub fn [<diff_apply_ $trait_name:snake>]<D, F>(&mut self, diff: Vec<D>, mut handler: F)
                where F: FnMut(&mut dyn $trait_name, &D)
                {
                    $crate::__world_define_visitors_common!(@da_use_entity_tuple $struct_name $trait_name $entity_tuple self diff handler);
                }

                /// clear all arenas whose element type implements this trait.
                /// No-op for arenas whose element types do not implement the trait.
                #[allow(unused)]
                pub fn [<clear_ $trait_name:snake>](&mut self) {
                    $crate::__world_define_visitors_common!(@clear_use_entity_tuple $struct_name $trait_name $entity_tuple self);
                }

                /// total number of elements across arenas whose element type implements trait
                #[allow(unused)]
                pub fn [<len_ $trait_name:snake>](&self) -> usize {
                    $crate::__world_define_visitors_common!(@len_use_entity_tuple $struct_name $trait_name $entity_tuple self)
                }

                /// get type erased arenas whose element type implements this trait
                #[allow(unused)]
                pub fn [<any_arenas_ $trait_name:snake>](&self) -> [&dyn $crate::ErasedArena; $crate::__world_define_visitors_common!(@any_arenas_count_use_entity_tuple $struct_name $trait_name $entity_tuple)] {
                    $crate::__world_define_visitors_common!(@any_arenas_use_entity_tuple $struct_name $trait_name $entity_tuple self)
                }

                /// get type erased arenas (mutable) whose element type implements this trait
                #[allow(unused)]
                pub fn [<any_arenas_mut_ $trait_name:snake>](&mut self) -> [&mut dyn $crate::ErasedArena; $crate::__world_define_visitors_common!(@any_arenas_count_use_entity_tuple $struct_name $trait_name $entity_tuple)] {
                    $crate::__world_define_visitors_common!(@any_arenas_mut_use_entity_tuple $struct_name $trait_name $entity_tuple self)
                }
            )*
        }
    };

    (@v_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::v_if_applicable(
                    &$self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@vk_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::vk_if_applicable(
                    &$self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@vm_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::mv_if_applicable(
                    &mut $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@vkm_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::vkm_if_applicable(
                    &mut $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@r_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $predicate:ident $default:ident) => {
        $crate::paste::paste! {
            $(
                if <() as [<$struct_name VisitIf $trait_name>]<$entity>>::ACTIVE {
                    <() as [<$struct_name VisitIf $trait_name>]<$entity>>::r_if_applicable::<_>(
                        &mut $self_ident.[<$entity:snake>],
                        &mut $predicate,
                    );
                } else {
                    if !$default {
                        $self_ident.[<$entity:snake>].clear();
                    }
                }
            )*
        }
    };

    (@d_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            let len = $self_ident.[<len_$trait_name:snake>]();
            // MaybeUninit required in case population of diff vec panics
            let mut out: Vec<std::mem::MaybeUninit<D>> = Vec::with_capacity(len);
            unsafe { out.set_len(len); }
            let mut offset = 0usize;
            $(
                let arena_len = <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                    & $self_ident.[<$entity:snake>],
                );

                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::d_if_applicable(
                    & $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                    &mut out[offset..offset + arena_len],
                );

                offset += arena_len;
            )*

            return unsafe { // convert MaybeUninit<Vec> to normal Vec
                let ptr = out.as_mut_ptr() as *mut D;
                let len = out.len();
                let cap = out.capacity();
                ::std::mem::forget(out);
                ::std::vec::Vec::from_raw_parts(ptr, len, cap)
            };
        }
    };

    (@dm_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            let len = $self_ident.[<len_$trait_name:snake>]();
            // MaybeUninit required in case population of diff vec panics
            let mut out: Vec<std::mem::MaybeUninit<D>> = Vec::with_capacity(len);
            unsafe { out.set_len(len); }
            let mut offset = 0usize;
            $(
                let arena_len = <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                    & $self_ident.[<$entity:snake>],
                );

                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::dm_if_applicable(
                    &mut $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                    &mut out[offset..offset + arena_len],
                );

                offset += arena_len;
            )*

            return unsafe { // convert MaybeUninit<Vec> to normal Vec
                let ptr = out.as_mut_ptr() as *mut D;
                let len = out.len();
                let cap = out.capacity();
                ::std::mem::forget(out);
                ::std::vec::Vec::from_raw_parts(ptr, len, cap)
            };
        }
    };

    (@da_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $diff_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            let mut offset = 0usize;
            $(
                let arena_len = <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                    & $self_ident.[<$entity:snake>],
                );

                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::da_if_applicable(
                    &mut $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                    &$diff_ident[offset..offset + arena_len],
                );

                offset += arena_len;
            )*
        }
    };

    (@clear_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::clear_if_applicable(
                    &mut $self_ident.[<$entity:snake>],
                );
            )*
        }
    };

    (@len_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident) => {
        $crate::paste::paste! {
            {
                let mut total = 0usize;
                $(
                    total += <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                        & $self_ident.[<$entity:snake>],
                    );
                )*
                total
            }
        }
    };

    (@any_arenas_count_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) ) => {
        $crate::paste::paste! {
            {
                0 $(+ if <() as [<$struct_name VisitIf $trait_name>]<$entity>>::ACTIVE {1} else {0})*
            }
        }
    };

    (@any_arenas_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident) => {
        $crate::paste::paste! {
            {
                const NUM_ARENAS: usize = 0 $(+ if <() as [<$struct_name VisitIf $trait_name>]<$entity>>::ACTIVE {1} else {0})*;
                let mut tmp: [std::mem::MaybeUninit<&dyn $crate::ErasedArena>; NUM_ARENAS] = unsafe { std::mem::MaybeUninit::uninit().assume_init() };
                let mut idx = 0;
                $(
                    if <() as [<$struct_name VisitIf $trait_name>]<$entity>>::ACTIVE {
                        tmp[idx] = std::mem::MaybeUninit::new(&$self_ident.[<$entity:snake>] as &dyn $crate::ErasedArena);
                        idx += 1;
                    }
                )*
                unsafe {
                    std::mem::transmute::<
                        [std::mem::MaybeUninit<&dyn $crate::ErasedArena>; NUM_ARENAS],
                        [&dyn $crate::ErasedArena; NUM_ARENAS]
                    >(tmp)
                }
            }
        }
    };

    (@any_arenas_mut_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident) => {
        $crate::paste::paste! {
            {
                const NUM_ARENAS: usize = 0 $(+ if <() as [<$struct_name VisitIf $trait_name>]<$entity>>::ACTIVE {1} else {0})*;
                let mut tmp: [std::mem::MaybeUninit<&mut dyn $crate::ErasedArena>; NUM_ARENAS] = unsafe { std::mem::MaybeUninit::uninit().assume_init() };
                let mut idx = 0;
                $(
                    if <() as [<$struct_name VisitIf $trait_name>]<$entity>>::ACTIVE {
                        tmp[idx] = std::mem::MaybeUninit::new(&mut $self_ident.[<$entity:snake>] as &mut dyn $crate::ErasedArena);
                        idx += 1;
                    }
                )*
                unsafe {
                    std::mem::transmute::<
                        [std::mem::MaybeUninit<&mut dyn $crate::ErasedArena>; NUM_ARENAS],
                        [&mut dyn $crate::ErasedArena; NUM_ARENAS]
                    >(tmp)
                }
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
#[cfg(not(feature = "rayon"))]
macro_rules! __world_define_visitors {
    // Non-rayon version simply forwards to the common macro
    (@pass_entity_tuple $struct_name:ident $($trait_name:ident),* @ $entity_tuple:tt) => {
        $crate::__world_define_visitors_common!(@pass_entity_tuple $struct_name $($trait_name),* @ $entity_tuple);
    };
}

#[doc(hidden)]
#[macro_export]
#[cfg(feature = "rayon")]
macro_rules! __world_define_visitors {
    // https://stackoverflow.com/a/37754096/15534181
    //
    // generate visit_* functions per trait
    (@pass_entity_tuple $struct_name:ident $($trait_name:ident),* @ $entity_tuple:tt) => {
        // non-parallel visit functions
        $crate::__world_define_visitors_common!(@pass_entity_tuple $struct_name $($trait_name),* @ $entity_tuple);

        // parallel visit functions (added only when rayon feature enabled)
        $crate::paste::paste! {
            $(
                /// in parallel, visit all entities that implement the trait
                #[allow(unused)]
                pub fn [<par_visit_ $trait_name:snake>]<F>(&self, handler: F)
                where
                    F: Fn(&dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@pv_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// in parallel, mutably visit all entities that implement the trait
                #[allow(unused)]
                pub fn [<par_visit_mut_ $trait_name:snake>]<F>(&mut self, handler: F)
                where
                    F: Fn(&mut dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@pvm_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// in parallel, visit all entities that implement the trait,
                /// with their keys
                /// 
                /// since the slotmap key is type agnostic, it's important to
                /// keep the arena id together with the slot map key (don't use
                /// the slot map key on the wrong slot map)
                #[allow(unused)]
                pub fn [<par_visit_key_ $trait_name:snake>]<F>(&self, handler: F)
                where
                    F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@pvk_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// in parallel, mutably visit all entities that implement the
                /// trait, with their keys
                ///
                /// since the slotmap key is type agnostic, it's important to
                /// keep the arena id together with the slot map key (don't use
                /// the slot map key on the wrong slot map)
                #[allow(unused)]
                pub fn [<par_visit_key_mut_ $trait_name:snake>]<F>(&mut self, handler: F)
                where
                    F: Fn(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@pvkm_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// forwards to par_retain_with_default with DEFAULT=true
                #[allow(unused)]
                pub fn [<par_retain_ $trait_name:snake>]<F>(&mut self, mut predicate: F)
                where
                    F: Fn(&mut dyn $trait_name) -> bool + Send + Sync
                {
                    self.[<par_retain_with_default_ $trait_name:snake>]::<true, F>(predicate)
                }

                /// WARN: this is parallel between arenas, not parallel between
                /// elements. this may be improved in a future version
                ///
                /// in parallel, retain entities matching predicate, removing
                /// those that don't.
                ///
                /// predicate applies only to entities implementing the trait.
                ///
                /// others instead use the DEFAULT value - if true entities not
                /// implementing trait are kept, false they are removed.
                #[allow(unused)]
                pub fn [<par_retain_with_default_ $trait_name:snake>]<const DEFAULT: bool, F>(&mut self, mut predicate: F)
                where
                    F: Fn(&mut dyn $trait_name) -> bool + Send + Sync
                {
                    $crate::__world_define_visitors!(@pr_use_entity_tuple $struct_name $trait_name $entity_tuple self predicate DEFAULT);
                }

                /// in parallel, produce a diff vector
                ///
                /// entities must not be inserted or removed between diff
                /// creation and application
                #[allow(unused)]
                pub fn [<par_diff_ $trait_name:snake>]<D, F>(&mut self, mut handler: F) -> Vec<D>
                where
                    D: Send,
                    F: Fn(&dyn $trait_name) -> D + Send + Sync
                {
                    $crate::__world_define_visitors!(@pd_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// in parallel, produce a diff vector from mutably viewing
                /// elements
                ///
                /// entities must not be inserted or removed between diff
                /// creation and application
                #[allow(unused)]
                pub fn [<par_diff_mut_ $trait_name:snake>]<D, F>(&mut self, mut handler: F) -> Vec<D>
                where
                    D: Send,
                    F: Fn(&mut dyn $trait_name) -> D + Send + Sync
                {
                    $crate::__world_define_visitors!(@pdm_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// in parallel, apply the diff vector.
                ///
                /// entities must not be inserted or removed between diff
                /// creation and application
                #[allow(unused)]
                pub fn [<par_diff_apply_ $trait_name:snake>]<D, F>(&mut self, diff: Vec<D>, handler: F)
                where
                    D: Sync,
                    F: Fn(&mut dyn $trait_name, &D) + Send + Sync
                {
                    $crate::__world_define_visitors!(@pda_use_entity_tuple $struct_name $trait_name $entity_tuple self diff handler);
                }
            )*
        }
    };

    (@pv_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
        use $crate::rayon::scope;
            scope(|s| {
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        let arena_ref = &$self_ident.[<$entity:snake>];
                        s.spawn(|_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pv_if_applicable(
                                arena_ref,
                                &$handler_ident,
                            );
                        });
                    }
                )*
            });
        }
    };

    (@pvk_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
        use $crate::rayon::scope;
            scope(|s| {
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        let arena_ref = &$self_ident.[<$entity:snake>];
                        s.spawn(|_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pvk_if_applicable(
                                arena_ref,
                                &$handler_ident,
                            );
                        });
                    }
                )*
            });
        }
    };

    (@pvm_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            use $crate::rayon::scope;
            scope(|s| {
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        s.spawn(|_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pvm_if_applicable(
                                &mut $self_ident.[<$entity:snake>],
                                &$handler_ident,
                            );
                        });
                    }
                )*
            });
        }
    };

    (@pvkm_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            use $crate::rayon::scope;
            scope(|s| {
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        s.spawn(|_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pvkm_if_applicable(
                                &mut $self_ident.[<$entity:snake>],
                                &$handler_ident,
                            );
                        });
                    }
                )*
            });
        }
    };

    (@pr_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $predicate_ident:ident $default:ident) => {
        $crate::paste::paste! {
            use $crate::rayon::scope;
            scope(|s| {
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        s.spawn(|_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pr_if_applicable::<_>(
                                &mut $self_ident.[<$entity:snake>],
                                &$predicate_ident,
                            );
                        });
                    } else {
                        if !$default {
                            $self_ident.[<$entity:snake>].clear();
                        }
                    }
                )*
            });
        }
    };

    (@pd_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            let len = $self_ident.[<len_$trait_name:snake>]();
            // MaybeUninit required in case population of diff vec panics
            let mut out: Vec<std::mem::MaybeUninit<D>> = Vec::with_capacity(len);
            unsafe { out.set_len(len); }
            use $crate::rayon::scope;
            scope(|s| {
                let mut remaining: &mut [_] = &mut out[..];
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        let arena_len = <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                            & $self_ident.[<$entity:snake>],
                        );

                        let (arena_slice, rest) = remaining.split_at_mut(arena_len);
                        remaining = rest;
                        let arena_ref = &$self_ident.[<$entity:snake>];
                        let handler_ref = &$handler_ident;
                        s.spawn(move |_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pd_if_applicable(
                                arena_ref,
                                handler_ref,
                                arena_slice,
                            );
                        });
                    }
                )*
            });

            return unsafe { // convert MaybeUninit<Vec> to normal Vec
                let ptr = out.as_mut_ptr() as *mut D;
                let len = out.len();
                let cap = out.capacity();
                ::std::mem::forget(out);
                ::std::vec::Vec::from_raw_parts(ptr, len, cap)
            };
        }
    };

    (@pdm_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            let len = $self_ident.[<len_$trait_name:snake>]();
            // MaybeUninit required in case population of diff vec panics
            let mut out: Vec<std::mem::MaybeUninit<D>> = Vec::with_capacity(len);
            unsafe { out.set_len(len); }
            use $crate::rayon::scope;
            scope(|s| {
                let mut remaining: &mut [_] = &mut out[..];
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        let arena_len = <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                            & $self_ident.[<$entity:snake>],
                        );

                        let (arena_slice, rest) = remaining.split_at_mut(arena_len);
                        remaining = rest;
                        let arena_ref = &mut $self_ident.[<$entity:snake>];
                        let handler_ref = &$handler_ident;
                        s.spawn(move |_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pdm_if_applicable(
                                arena_ref,
                                handler_ref,
                                arena_slice,
                            );
                        });
                    }
                )*
            });

            return unsafe { // convert MaybeUninit<Vec> to normal Vec
                let ptr = out.as_mut_ptr() as *mut D;
                let len = out.len();
                let cap = out.capacity();
                ::std::mem::forget(out);
                ::std::vec::Vec::from_raw_parts(ptr, len, cap)
            };
        }
    };

    (@pda_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $diff_ident:ident $handler_ident:ident) => {
    $crate::paste::paste! {
        use $crate::rayon::scope;
        scope(|s| {
            let mut remaining: &[_] = &$diff_ident[..];
            $(
                if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                    let arena_len = <() as [<$struct_name VisitIf $trait_name>]<$entity>>::len_if_applicable(
                        & $self_ident.[<$entity:snake>],
                    );

                    let (arena_slice, rest) = remaining.split_at(arena_len);
                    remaining = rest;
                    let arena_ref = &mut $self_ident.[<$entity:snake>];
                    let handler_ref = &$handler_ident;

                    s.spawn(move |_| {
                        <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pda_if_applicable(
                            arena_ref,
                            handler_ref,
                            arena_slice,
                        );
                    });
                }
            )*
        });
    }
};
}

#[doc(hidden)]
#[macro_export]
#[cfg(not(feature = "debug"))]
macro_rules! __world_define_struct {
    ($struct_name:ident, $( $entity:ty ),*) => {
        $crate::paste::paste! {
            #[derive(Default)]
            #[allow(private_interfaces)] // member is pub even if underlying type isn't
            pub struct $struct_name {
                $(
                    pub [<$entity:snake>]: $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity>,
                )*
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
#[cfg(feature = "debug")]
macro_rules! __world_define_struct {
    ($struct_name:ident, $( $entity:ty ),*) => {
        $crate::paste::paste! {
            #[derive(Default, Debug)]
            #[allow(private_interfaces)] // member is pub even if underlying type isn't
            pub struct $struct_name {
                $(
                    pub [<$entity:snake>]: $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity>,
                )*
            }
        }
    };
}

#[doc(hidden)]
#[macro_export]
#[cfg(not(feature = "serde"))]
macro_rules! __world_serde_support {
    ($struct_name:ident $( $entity:ty ),*) => {};
}

#[doc(hidden)]
#[macro_export]
#[cfg(feature = "serde")]
macro_rules! __world_serde_support {
    ($struct_name:ident $( $entity:ty ),*) => {
        $crate::paste::paste! {

impl $crate::serde::Serialize for [<$struct_name ArenaID>] {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: $crate::serde::Serializer,
    {
        let s = match self.0 {
            $(
                i if i == $struct_name::arena_id::<$entity>().0 => stringify!($entity),
            )*
            // impossible! could be a panic here instead
            _ => return Err($crate::serde::ser::Error::custom(format!("Unknown ArenaID {}", self.0))),
        };
        serializer.serialize_str(s)
    }
}

impl<'de> $crate::serde::Deserialize<'de> for [<$struct_name ArenaID>] {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: $crate::serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let id = match s.as_str() {
            $(
                stringify!($entity) => $struct_name::arena_id::<$entity>().0,
            )*
            // possible! suppose a different world loads it in, and there isn't that type.
            _ => return Err($crate::serde::de::Error::custom(format!("Unknown ArenaID string {}", s))),
        };
        Ok([<$struct_name ArenaID>](id))
    }
}

impl $crate::serde::ser::Serialize for $struct_name {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: $crate::serde::ser::Serializer,
    {
        use $crate::SerdeArena;
        use $crate::serde::ser::SerializeStruct;

        let field_count = 0 $( + if <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as $crate::SerdeArena<'static>>::ACTIVE { 1 } else { 0 } )*;
        let mut state = serializer.serialize_struct(
            stringify!($struct_name),
            field_count
        )?;
        $(
            self.[<$entity:snake>].serialize_arena(stringify!($entity), &mut state)?;
        )*
        state.end()
    }
}

// build list of serde members in world
static [<$struct_name:upper _DESERIALIZE_FIELDS>]: &'static [&'static str] = &{
    const TMP: [Option<&'static str>; { 0 $(+ { let _ = stringify!($entity); 1 })* }] = [
        $(
            if <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity>
                as $crate::SerdeArena<'static>>::ACTIVE {
                Some(stringify!($entity))
            } else {
                None
            }
        ),*
    ];

    const COUNT: usize = 0 $(+ if (<$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity>
        as $crate::SerdeArena<'static>>::ACTIVE) {1} else {0})*;

    let mut arr: [&'static str; COUNT] = [""; COUNT];
    let mut j = 0;
    let mut k = 0;
    while j < TMP.len() {
        if let Some(s) = TMP[j] {
            arr[k] = s;
            k += 1;
        }
        j += 1;
    }

    arr
};

impl<'de> $crate::serde::Deserialize<'de> for $struct_name {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: $crate::serde::Deserializer<'de>,
    {
        use $crate::serde::de::{MapAccess, SeqAccess, Visitor, Error};
        use std::fmt;

        #[derive($crate::serde::Deserialize)]
        #[serde(field_identifier)]
        enum Field { $($entity),* }

        struct WorldVisitor;

        impl<'de> Visitor<'de> for WorldVisitor {
            type Value = $struct_name;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "struct {}", stringify!($struct_name))
            }

            fn visit_map<V>(self, mut map: V) -> Result<$struct_name, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut world = $struct_name::default();

                while let Some(key) = map.next_key::<Field>()? {
                    match key {
                        $(
                            Field::$entity => {
                                world.[<$entity:snake>] =
                                    <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity>
                                        as $crate::SerdeArena<'de>>::deserialize_arena(&mut map)?;
                            }
                        )*
                    }
                }

                Ok(world)
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<$struct_name, V::Error>
            where
                V: SeqAccess<'de>,
            {
                Ok($struct_name {
                    $(
                        [<$entity:snake>]: <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity>
                            as $crate::SerdeArena<'de>>::from_seq(&mut seq, stringify!($entity))?,
                    )*
                })
            }
        }

        deserializer.deserialize_struct(
            stringify!($struct_name),
            &[<$struct_name:upper _DESERIALIZE_FIELDS>],
            WorldVisitor,
        )
    }
}

        }
    };
}

/// ```ignore
/// entity_trait_system::world!(MyWorld, Enemy, Player; TestTrait, SecondTestTrait);
/// ```
/// 
/// This defines two structs:
///  - the world called `MyWorld`. It contains an arena for each entity type,
///    and generates query functions based on the supplied traits.
///  - the id type `MyWorldArenaId`. It's a stable equivalent to `TypeId`, which
///    serializes to the name of an entity's type. It can be used as the key
///    when accessing arenas in a type erased way.
#[macro_export]
macro_rules! world {
    // main macro form: define struct + traits + impl
    (
        // the name of the struct being defined
        $struct_name:ident, // the types of entities which can exist in the world
        $( $entity:ty ),* $(,)? // optional trailing comma
        ;// semi colon separator between lists
        // the traits which are query-able over all types in the world
        $( $trait_name:ident ),* $(,)? // optional trailing comma
    ) => {
        $crate::paste::paste! {

/// the world is composed of arenas. this selects which arena.
///
/// unlike TypeID, this is stable across rust versions. It serializes to the
/// name of the entity's type.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct [<$struct_name ArenaID>](usize);

$crate::__world_define_struct!($struct_name, $($entity),*);

$(
    trait [<$struct_name VisitIf $trait_name>]<T> {
        fn v_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(&dyn $trait_name);

        fn mv_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(&mut dyn $trait_name);

        fn vk_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name);

        fn vkm_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name);

        fn r_if_applicable<P>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, predicate: P)
        where
            P: FnMut(&mut dyn $trait_name) -> bool;

        fn d_if_applicable<D, F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F, out: &mut [std::mem::MaybeUninit<D>])
        where
            F: FnMut(&dyn $trait_name) -> D;

        fn dm_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F, out: &mut [std::mem::MaybeUninit<D>])
        where
            F: FnMut(&mut dyn $trait_name) -> D;
        
        fn da_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F, i: &[D])
        where
            F: FnMut(&mut dyn $trait_name, &D);

        fn clear_if_applicable(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>);

        fn len_if_applicable(arena: & $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>) -> usize;

        const ACTIVE: bool;
    }

    // no-op for types not implementing the trait
    impl<T> [<$struct_name VisitIf $trait_name>]<T> for () {
        default fn v_if_applicable<F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(&dyn $trait_name) {}

        default fn mv_if_applicable<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(&mut dyn $trait_name) {}

        default fn vk_if_applicable<F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name) {}

        default fn vkm_if_applicable<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name) {}

        default fn r_if_applicable<P>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _predicate: P)
        where P: FnMut(&mut dyn $trait_name) -> bool {}

        default fn d_if_applicable<D, F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F, _out: &mut [std::mem::MaybeUninit<D>])
        where
            F: FnMut(&dyn $trait_name) -> D {}

        default fn dm_if_applicable<D, F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F, _out: &mut [std::mem::MaybeUninit<D>])
        where
            F: FnMut(&mut dyn $trait_name) -> D {}

        default fn da_if_applicable<D, F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F, _i: &[D])
        where
            F: FnMut(&mut dyn $trait_name, &D) {}

        default fn clear_if_applicable(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>) {}

        default fn len_if_applicable(_arena: & $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>) -> usize { 0 }

        default const ACTIVE: bool = false;
    }

    impl<T: $trait_name> [<$struct_name VisitIf $trait_name>]<T> for () {
        fn v_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(&dyn $trait_name)
        {
            arena
                .values_as_slice()
                .iter()
                .for_each(|entity| handler(entity));
        }

        fn mv_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(&mut dyn $trait_name)
        {
            arena
                .values_as_mut_slice()
                .iter_mut()
                .for_each(|entity| handler(entity));
        }

        fn r_if_applicable<P>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut predicate: P)
        where P: FnMut(&mut dyn $trait_name) -> bool
        {
            arena.retain(|_, entity| predicate(entity));
        }

        fn d_if_applicable<D, F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F, out: &mut [std::mem::MaybeUninit<D>])
        where
            F: FnMut(&dyn $trait_name) -> D {
                for (e, out_slot) in arena.values_as_slice().iter().zip(out.iter_mut()) {
                    *out_slot = std::mem::MaybeUninit::new(handler(e));
                }
            }

        fn dm_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F, out: &mut [std::mem::MaybeUninit<D>])
        where
            F: FnMut(&mut dyn $trait_name) -> D {
                for (e, out_slot) in arena.values_as_mut_slice().iter_mut().zip(out.iter_mut()) {
                    *out_slot = std::mem::MaybeUninit::new(handler(e));
                }
            }
        
        fn da_if_applicable<D, F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F, i: &[D])
        where
            F: FnMut(&mut dyn $trait_name, &D) {
                for (e, in_value) in arena.values_as_mut_slice().iter_mut().zip(i.iter()) {
                    handler(e, in_value);
                }
            }

        fn vk_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &dyn $trait_name)
        {
            let keys = arena.keys_as_slice();
            let values = arena.values_as_slice();
            keys.iter()
                .zip(values.iter())
                .for_each(|(k, v)| handler(($struct_name::arena_id::<T>(), *k), v));
        }

        fn vkm_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(([<$struct_name ArenaID>], $crate::slotmap::DefaultKey), &mut dyn $trait_name)
        {
            let (keys, values) = arena.keys_values_as_mut_slices();
            keys.iter()
                .zip(values.iter_mut())
                .for_each(|(k, v)| handler(($struct_name::arena_id::<T>(), *k), v));
        }

        fn clear_if_applicable(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>) {
            arena.clear();
        }

        fn len_if_applicable(arena: & $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>) -> usize {
            arena.len()
        }

        default const ACTIVE: bool = true;
    }

)*

impl $struct_name {
    $crate::__world_define_visitors!(@pass_entity_tuple $struct_name $($trait_name),* @ ($($entity),*));

    /// access underlying arena for type T
    /// ```
    /// world.arena::<Enemy>().len()
    /// ```
    #[allow(unused)]
    pub fn arena<T>(&self) -> &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T> {
        use $crate::ArenaCast;
        $(
            if <T as $crate::IsType<$entity>>::VALUE {
                return <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as ArenaCast<T>>::cast(&self.[<$entity:snake>]);
            }
        )* // checked compiler explorer to be sure - all of this is constant folded with optimizations enabled
        panic!("In call to {}::arena::<{}>(), {} not registered", stringify!($struct_name), std::any::type_name::<T>(), std::any::type_name::<T>());
    }

    /// mutably access underlying arena for type T
    /// ```
    /// world.arena::<Enemy>().len()
    /// ```
    #[allow(unused)]
    pub fn arena_mut<T>(&mut self) -> &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T> {
        use $crate::ArenaCast;
        $(
            if <T as $crate::IsType<$entity>>::VALUE {
                return <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as ArenaCast<T>>::cast_mut(&mut self.[<$entity:snake>]);
            }
        )*
        panic!("In call to {}::arena_mut::<{}>(), {} not registered", stringify!($struct_name), std::any::type_name::<T>(), std::any::type_name::<T>());
    }

    /// get the ArenaID for type T
    ///
    /// this is used as part of the type erased api
    /// ```
    /// let arena_id = MyWorld::arena_id::<Enemy>();
    /// let arena = world.any_arena(arena_id);
    /// ```
    #[allow(unused)]
    pub fn arena_id<T>() -> [<$struct_name ArenaID>] {
        let mut i = 0usize;
        $(
            if <T as $crate::IsType<$entity>>::VALUE {
                return [<$struct_name ArenaID>](i);
            }
            i += 1;
        )*
        panic!("In call to {}::arena_id::<{}>(), {} not registered", stringify!($struct_name), std::any::type_name::<T>(), std::any::type_name::<T>());
    }

    /// type erased API. get arena by ArenaID, returning trait object
    #[allow(unused)]
    pub fn any_arena(&self, which: [<$struct_name ArenaID>]) -> &dyn $crate::ErasedArena {
        match which.0 {
            $(
                i if i == Self::arena_id::<$entity>().0 => &self.[<$entity:snake>] as &dyn $crate::ErasedArena,
            )*
            _ => panic!("No arena for type id {}", which.0),
        }
    }

    /// type erased API. get mutable arena by ArenaID, returning trait object
    #[allow(unused)]
    pub fn any_arena_mut(&mut self, which: [<$struct_name ArenaID>]) -> &mut dyn $crate::ErasedArena {
        match which.0 {
            $(
                i if i == Self::arena_id::<$entity>().0 => &mut self.[<$entity:snake>] as &mut dyn $crate::ErasedArena,
            )*
            _ => panic!("No mutable arena for type id {}", which.0),
        }
    }

    #[allow(unused)]
    pub fn clear(&mut self) {
        $(
            self.[<$entity:snake>].clear();
        )*
    }

    #[allow(unused)]
    pub fn len(&self) -> usize {
        0 $( + self.[<$entity:snake>].len() )*
    }
}

$crate::__world_serde_support!($struct_name $($entity),*);

$crate::__world_define_rayon_trait_helpers!($struct_name $($trait_name),*);

        }
    };
}
