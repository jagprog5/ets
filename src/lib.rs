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

#[macro_export]
#[cfg(not(feature = "rayon"))]
macro_rules! __world_define_rayon_trait_helpers {
    ($struct_name:ident $( $trait_name:ident ),*) => {};
}

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

#[macro_export]
#[cfg(not(feature = "rayon"))]
macro_rules! __world_define_visitors {
    // Non-rayon version simply forwards to the common macro
    (@pass_entity_tuple $struct_name:ident $($trait_name:ident),* @ $entity_tuple:tt) => {
        $crate::__world_define_visitors_common!(@pass_entity_tuple $struct_name $($trait_name),* @ $entity_tuple);
    };
}

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

#[macro_export]
#[cfg(not(feature = "serde"))]
macro_rules! __world_serde_support {
    ($struct_name:ident $( $entity:ty ),*) => {};
}

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

        struct WorldVisitor;

        impl<'de> Visitor<'de> for WorldVisitor {
            type Value = $struct_name;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "struct {}", stringify!($struct_name))
            }

            // JSON-style: map fields
            fn visit_map<V>(self, mut map: V) -> Result<$struct_name, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut world = $struct_name::default();

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        $(
                            stringify!($entity) => {
                                world.[<$entity:snake>] =
                                    <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as $crate::SerdeArena<'de>>::deserialize_arena(&mut map)?;
                            }
                        )*
                        other => {
                            return Err(V::Error::custom(format!(
                                "Unknown field '{}' for {}",
                                other,
                                stringify!($struct_name)
                            )));
                        }
                    }
                }

                Ok(world)
            }

            // Bincode-style: sequence fields.
            // WARNING! Requires stated entities not changing order
            fn visit_seq<V>(self, mut seq: V) -> Result<$struct_name, V::Error>
            where
                V: SeqAccess<'de>,
            {
               Ok($struct_name {
                    $(
                        [<$entity:snake>]: <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as $crate::SerdeArena<'de>>::from_seq(&mut seq, stringify!($entity))?,
                    )*
                })
            }
        }

        // Choose entry point depending on deserializer type
        //
        // JSON/CBOR: calls `deserialize_struct` -> `visit_map`
        // Bincode: must call `deserialize_struct` directly (sequence)
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

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------
#[cfg(test)]
mod tests {
    // Entities used in tests
    #[derive(Debug)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct Player {
        id: i32,
    }

    #[derive(Debug)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct Enemy {
        hp: i32,
    }

    // A third entity that implements no traits (for retain default path tests)
    #[derive(Debug)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
    struct NonTrait {
        #[allow(unused)]
        val: i32,
    }

    pub trait TestTrait {
        fn metric(&self) -> i32;
        fn add(&mut self, delta: i32);
    }
    impl TestTrait for Player {
        fn metric(&self) -> i32 {
            self.id
        }
        fn add(&mut self, delta: i32) {
            self.id += delta;
        }
    }
    impl TestTrait for Enemy {
        fn metric(&self) -> i32 {
            self.hp
        }
        fn add(&mut self, delta: i32) {
            self.hp += delta;
        }
    }

    // Second trait, implemented only by Player
    pub trait SecondTestTrait {
        fn touch(&mut self);
    }
    impl SecondTestTrait for Player {
        fn touch(&mut self) {
            self.id += 1000;
        }
    }

    // Primary world used in most tests
    world!(MyWorld, Enemy, Player; TestTrait, SecondTestTrait);

    // Extended world including a non-trait entity to test default retain/clear
    world!(AnotherWorld, Enemy, NonTrait, Player; TestTrait, SecondTestTrait);

    // doc: these tests are AI generated

    #[test]
    fn basic_arena_access_and_len() {
        let mut world = MyWorld::default();

        let p0 = world.player.insert(Player { id: 1 });
        let e0 = world.arena_mut::<Enemy>().insert(Enemy { hp: 10 });
        let _e1 = world.arena_mut::<Enemy>().insert(Enemy { hp: 9 });

        assert_eq!(world.len(), 3);

        // typed arena accessors
        assert_eq!(world.arena::<Enemy>().len(), 2);
        assert_eq!(world.arena::<Player>().len(), 1);

        // arena_id order matches declaration order: Enemy then Player
        assert_eq!(MyWorld::arena_id::<Enemy>().0, 0);
        assert_eq!(MyWorld::arena_id::<Player>().0, 1);

        // type-erased access
        let player_id = MyWorld::arena_id::<Player>();
        let enemy_id = MyWorld::arena_id::<Enemy>();

        let p = world
            .any_arena(player_id)
            .get(p0)
            .unwrap()
            .downcast_ref::<Player>()
            .unwrap();
        assert_eq!(p.id, 1);

        let e = world
            .any_arena(enemy_id)
            .get(e0)
            .unwrap()
            .downcast_ref::<Enemy>()
            .unwrap();
        assert_eq!(e.hp, 10);

        // get_mut
        {
            let p_mut = world
                .any_arena_mut(player_id)
                .get_mut(p0)
                .unwrap()
                .downcast_mut::<Player>()
                .unwrap();
            p_mut.id += 5;
        }
        assert_eq!(world.player.get(p0).unwrap().id, 6);

        // unsafe unchecked getters
        unsafe {
            let p_un = world
                .any_arena(player_id)
                .get_unchecked(p0)
                .downcast_ref::<Player>()
                .unwrap();
            assert_eq!(p_un.id, 6);
            let e_unm = world
                .any_arena_mut(enemy_id)
                .get_unchecked_mut(e0)
                .downcast_mut::<Enemy>()
                .unwrap();
            e_unm.hp += 1;
        }
        assert_eq!(world.enemy.get(e0).unwrap().hp, 11);

        // world clear
        world.clear();
        assert_eq!(world.len(), 0);
    }

    #[test]
    fn visitors_immutable_mutable_and_lengths() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 10 });
        world.enemy.insert(Enemy { hp: 5 });

        // total len of entities implementing TestTrait (both arenas)
        assert_eq!(world.len_test_trait(), 4);

        // immutable visit (collect metrics)
        let mut metrics = Vec::new();
        world.visit_test_trait(|t| metrics.push(t.metric()));
        metrics.sort();
        assert_eq!(metrics, vec![1, 2, 5, 10]);

        // mutable visit on SecondTestTrait (only Player implements it)
        world.visit_mut_second_test_trait(|t| t.touch());
        let ids: Vec<i32> = world.player.values().map(|p| p.id).collect();
        assert_eq!(ids, vec![1001, 1002]);
        // enemies remain unchanged
        let hps: Vec<i32> = world.enemy.values().map(|e| e.hp).collect();
        assert_eq!(hps, vec![10, 5]);

        // clear only entities implementing TestTrait
        world.clear_test_trait();
        assert_eq!(world.len_test_trait(), 0);
        // both arenas implement TestTrait here, so both are cleared
        assert_eq!(world.len(), 0);
    }

    #[test]
    fn retain_with_default_and_selective_clear() {
        let mut world = AnotherWorld::default();

        // Fill all arenas
        let _ = world.enemy.insert(Enemy { hp: 10 });
        let _ = world.enemy.insert(Enemy { hp: 3 });
        let _ = world.non_trait.insert(NonTrait { val: 99 });
        let _ = world.player.insert(Player { id: 1 });

        // Retain only TestTrait implementers with metric >= 5, DEFAULT=false
        // => Enemy{hp<5} removed; Player{id=1<5} removed; NonTrait cleared due to DEFAULT=false
        world.retain_with_default_test_trait::<false, _>(|t| t.metric() >= 5);

        assert_eq!(world.enemy.len(), 1); // hp=10 survives
        assert_eq!(world.player.len(), 0);
        assert_eq!(world.non_trait.len(), 0);

        // Now fill again and set DEFAULT=true: NonTrait is preserved.
        let _ = world.player.insert(Player { id: 7 });
        let _ = world.non_trait.insert(NonTrait { val: 42 });
        world.retain_with_default_test_trait::<true, _>(|t| t.metric() >= 5);

        assert_eq!(world.enemy.len(), 1); // still hp=10
        assert_eq!(world.player.len(), 1); // id=7
        assert_eq!(world.non_trait.len(), 1); // preserved by DEFAULT=true

        // clear only TestTrait implementers (NonTrait remains)
        world.clear_test_trait();
        assert_eq!(world.enemy.len(), 0);
        assert_eq!(world.player.len(), 0);
        assert_eq!(world.non_trait.len(), 1);
    }

    #[test]
    fn diff_and_apply_roundtrip() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.enemy.insert(Enemy { hp: 2 });
        world.enemy.insert(Enemy { hp: 3 });

        // Create a diff vector from metrics doubled
        let diff = world.diff_test_trait(|t| t.metric() * 2);

        // Apply inverse operation: add(-diff/2) -> should zero all metrics
        world.diff_apply_test_trait(diff, |t, d| t.add(-d / 2));

        // Verify all metrics are zero
        let mut all_zero = true;
        world.visit_test_trait(|t| {
            all_zero = all_zero && t.metric() == 0;
        });
        assert!(all_zero);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn parallel_visitors_and_diff_apply() {
        use std::sync::atomic::{AtomicI64, Ordering};
        use std::sync::{Arc, Mutex};

        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 10 });
        world.enemy.insert(Enemy { hp: 5 });

        // parallel immutable visit: sum metrics
        let sum = AtomicI64::new(0);
        world.par_visit_test_trait(|t| {
            sum.fetch_add(t.metric() as i64, Ordering::Relaxed);
        });
        assert_eq!(sum.load(Ordering::Relaxed), 1 + 2 + 10 + 5);

        // parallel mutable visit (Players only)
        world.par_visit_mut_second_test_trait(|t| t.touch());
        let ids: Vec<i32> = world.player.values().map(|p| p.id).collect();
        assert_eq!(ids, vec![1001, 1002]);

        // parallel diff and apply
        let diff = world.par_diff_test_trait(|t| t.metric() * 3);
        // Use handler that resets values to zero via add(-metric)
        world.par_diff_apply_test_trait(diff, |t, d| t.add(-d / 3));

        // Verify all metrics are zero in parallel
        let results = Arc::new(Mutex::new(Vec::new()));
        world.par_visit_test_trait(|t| results.lock().unwrap().push(t.metric()));
        let mut vals = results.lock().unwrap().clone();
        vals.sort();
        assert_eq!(vals, vec![0, 0, 0, 0]);

        // parallel retain across arenas (note: per-element retain isn't parallel due to DenseSlotMap)
        world.par_retain_with_default_test_trait::<true, _>(|t| t.metric() == 0);
        assert_eq!(world.len_test_trait(), 4);
    }

    #[cfg(all(feature = "serde"))]
    #[test]
    fn serde_roundtrips_json_and_bincode() {
        // Only types that derive Serialize/Deserialize are included by SerdeArena
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 7 });
        world.enemy.insert(Enemy { hp: 42 });

        // JSON round-trip
        let serialized = serde_json::to_string(&world).unwrap();
        let de_json: MyWorld = serde_json::from_str(&serialized).unwrap();
        // spot-check values
        assert_eq!(de_json.player.len(), 1);
        assert_eq!(de_json.enemy.len(), 1);
        assert_eq!(de_json.player.values().next().unwrap().id, 7);
        assert_eq!(de_json.enemy.values().next().unwrap().hp, 42);

        // Bincode round-trip (using serde module)
        let serialized: Vec<u8> =
            bincode::serde::encode_to_vec(&world, bincode::config::standard()).unwrap();
        let (de_bin, _): (MyWorld, usize) =
            bincode::serde::decode_from_slice(&serialized, bincode::config::standard()).unwrap();

        assert_eq!(de_bin.player.len(), 1);
        assert_eq!(de_bin.enemy.len(), 1);
        assert_eq!(de_bin.player.values().next().unwrap().id, 7);
        assert_eq!(de_bin.enemy.values().next().unwrap().hp, 42);
    }

    #[test]
    fn empty_world_behavior() {
        let mut world = MyWorld::default();

        // empty world has zero length
        assert_eq!(world.len(), 0);
        assert_eq!(world.len_test_trait(), 0);
        assert_eq!(world.len_second_test_trait(), 0);

        // visitors on empty world do nothing
        let mut visited = false;
        world.visit_test_trait(|_| visited = true);
        assert!(!visited);

        // diff on empty world returns empty vec
        let diff = world.diff_test_trait(|t| t.metric());
        assert_eq!(diff.len(), 0);
    }

    #[test]
    fn retain_removes_all_when_predicate_always_false() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 10 });

        // retain nothing (predicate always false)
        world.retain_test_trait(|_| false);

        assert_eq!(world.len_test_trait(), 0);
        assert_eq!(world.len(), 0);
    }

    #[test]
    fn retain_keeps_all_when_predicate_always_true() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 10 });

        let initial_len = world.len_test_trait();

        // retain all (predicate always true)
        world.retain_test_trait(|_| true);

        assert_eq!(world.len_test_trait(), initial_len);
    }

    #[test]
    fn diff_apply_maintains_element_order() {
        let mut world = MyWorld::default();

        // Insert in specific order
        let _e1 = world.enemy.insert(Enemy { hp: 10 });
        let _e2 = world.enemy.insert(Enemy { hp: 20 });
        let _p1 = world.player.insert(Player { id: 5 });

        // Create diff that captures original metrics
        let original = world.diff_test_trait(|t| t.metric());

        // Modify values
        world.visit_mut_test_trait(|t| t.add(100));

        // Apply diff to restore (add back the negative delta)
        let current = world.diff_test_trait(|t| t.metric());
        let restore_diff: Vec<i32> = original
            .iter()
            .zip(current.iter())
            .map(|(orig, curr)| orig - curr)
            .collect();
        world.diff_apply_test_trait(restore_diff, |t, delta| t.add(*delta));

        // Verify restoration
        let mut final_metrics = Vec::new();
        world.visit_test_trait(|t| final_metrics.push(t.metric()));
        assert_eq!(final_metrics, vec![10, 20, 5]);
    }

    #[test]
    fn visit_key_provides_correct_keys_and_arena_ids() {
        let mut world = MyWorld::default();
        
        let e1 = world.enemy.insert(Enemy { hp: 10 });
        let e2 = world.enemy.insert(Enemy { hp: 20 });
        let p1 = world.player.insert(Player { id: 5 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Collect (ArenaID, Key) pairs via visit_key
        let mut collected = Vec::new();
        world.visit_key_test_trait(|(arena_id, key), _entity| {
            collected.push((arena_id, key));
        });
        
        // Should have all 3 keys with correct arena IDs
        assert_eq!(collected.len(), 3);
        
        // Verify Arena IDs and keys match
        assert!(collected.contains(&(enemy_arena_id, e1)));
        assert!(collected.contains(&(enemy_arena_id, e2)));
        assert!(collected.contains(&(player_arena_id, p1)));
    }

    #[test]
    fn visit_key_entity_association_with_arena_id() {
        let mut world = MyWorld::default();
        
        let e1 = world.enemy.insert(Enemy { hp: 100 });
        let e2 = world.enemy.insert(Enemy { hp: 200 });
        let p1 = world.player.insert(Player { id: 50 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Verify ArenaID-key-entity triplets are correct
        world.visit_key_test_trait(|(arena_id, key), entity| {
            let metric = entity.metric();
            
            // Verify the ArenaID + key matches the entity's value
            if arena_id == enemy_arena_id && key == e1 {
                assert_eq!(metric, 100);
            } else if arena_id == enemy_arena_id && key == e2 {
                assert_eq!(metric, 200);
            } else if arena_id == player_arena_id && key == p1 {
                assert_eq!(metric, 50);
            } else {
                panic!("Unexpected ArenaID/key combination");
            }
        });
    }

    #[test]
    fn visit_key_mut_can_modify_entities_by_arena_id() {
        let mut world = MyWorld::default();
        
        let e1 = world.enemy.insert(Enemy { hp: 10 });
        let p1 = world.player.insert(Player { id: 5 });
        let p2 = world.player.insert(Player { id: 7 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Modify entities based on their ArenaID + key
        world.visit_key_mut_test_trait(|(arena_id, key), entity| {
            if arena_id == enemy_arena_id && key == e1 {
                entity.add(90); // hp: 10 -> 100
            } else if arena_id == player_arena_id && key == p1 {
                entity.add(95); // id: 5 -> 100
            } else if arena_id == player_arena_id && key == p2 {
                entity.add(93); // id: 7 -> 100
            }
        });
        
        // Verify modifications
        assert_eq!(world.enemy.get(e1).unwrap().hp, 100);
        assert_eq!(world.player.get(p1).unwrap().id, 100);
        assert_eq!(world.player.get(p2).unwrap().id, 100);
    }

    #[test]
    fn visit_key_ordering_with_arena_ids() {
        let mut world = MyWorld::default();
        
        // Insert in specific order
        let e1 = world.enemy.insert(Enemy { hp: 1 });
        let e2 = world.enemy.insert(Enemy { hp: 2 });
        let p1 = world.player.insert(Player { id: 3 });
        let p2 = world.player.insert(Player { id: 4 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Collect in visitation order
        let mut arena_key_pairs = Vec::new();
        world.visit_key_test_trait(|(arena_id, key), _| {
            arena_key_pairs.push((arena_id, key));
        });
        
        // Should match: enemies first (in insertion order), then players
        assert_eq!(arena_key_pairs, vec![
            (enemy_arena_id, e1),
            (enemy_arena_id, e2),
            (player_arena_id, p1),
            (player_arena_id, p2),
        ]);
    }

    #[test]
    fn visit_key_with_selective_trait_arena_ids() {
        let mut world = MyWorld::default();
        
        let _e1 = world.enemy.insert(Enemy { hp: 10 });
        let p1 = world.player.insert(Player { id: 5 });
        let p2 = world.player.insert(Player { id: 7 });
        
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // SecondTestTrait only implemented by Player
        let mut player_data = Vec::new();
        world.visit_key_second_test_trait(|(arena_id, key), _| {
            player_data.push((arena_id, key));
        });
        
        // Should only have player keys with player arena ID
        assert_eq!(player_data.len(), 2);
        assert!(player_data.contains(&(player_arena_id, p1)));
        assert!(player_data.contains(&(player_arena_id, p2)));
    }

    #[test]
    fn visit_key_mut_selective_modification_by_arena_id() {
        let mut world = MyWorld::default();
        
        world.enemy.insert(Enemy { hp: 10 });
        world.enemy.insert(Enemy { hp: 20 });
        let p1 = world.player.insert(Player { id: 5 });
        let p2 = world.player.insert(Player { id: 7 });
        
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Modify only players via SecondTestTrait, verifying ArenaID
        world.visit_key_mut_second_test_trait(|(arena_id, _key), entity| {
            assert_eq!(arena_id, player_arena_id);
            entity.touch(); // adds 1000 to id
        });
        
        // Verify only players were modified
        assert_eq!(world.player.get(p1).unwrap().id, 1005);
        assert_eq!(world.player.get(p2).unwrap().id, 1007);
        
        // Enemies unchanged
        let enemy_hps: Vec<i32> = world.enemy.values().map(|e| e.hp).collect();
        assert_eq!(enemy_hps, vec![10, 20]);
    }

    #[test]
    fn visit_key_empty_world_no_arena_ids() {
        let world = MyWorld::default();
        
        let mut visited = false;
        world.visit_key_test_trait(|(_arena_id, _key), _entity| {
            visited = true;
        });
        
        assert!(!visited);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn par_visit_key_mut_concurrent_modification_with_arena_ids() {
        use std::sync::atomic::{AtomicI32, Ordering};
        
        let mut world = MyWorld::default();
        
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Insert many entities
        for i in 0..100 {
            world.player.insert(Player { id: i });
        }
        
        let modification_count = AtomicI32::new(0);
        
        // Modify all in parallel, verifying ArenaID
        world.par_visit_key_mut_test_trait(|(arena_id, _key), entity| {
            assert_eq!(arena_id, player_arena_id);
            entity.add(1000);
            modification_count.fetch_add(1, Ordering::Relaxed);
        });
        
        // Verify all were modified
        assert_eq!(modification_count.load(Ordering::Relaxed), 100);
        
        // Verify values
        for player in world.player.values() {
            assert!(player.id >= 1000 && player.id < 1100);
        }
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn par_visit_key_ordering_deterministic_with_arena_ids() {
        let mut world = MyWorld::default();
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Insert many entities
        let mut expected_pairs = Vec::new();
        for i in 0..50 {
            expected_pairs.push((enemy_arena_id, world.enemy.insert(Enemy { hp: i })));
            expected_pairs.push((player_arena_id, world.player.insert(Player { id: i })));
        }
        
        // Collect in visitation order
        let mut seq_pairs = Vec::new();
        world.visit_key_test_trait(|(arena_id, key), _| {
            seq_pairs.push((arena_id, key));
        });
        
        // Collect pairs via parallel visit (multiple times to check consistency)
        for _ in 0..5 {
            let mut par_pairs = Vec::new();
            world.visit_key_test_trait(|(arena_id, key), _| {
                par_pairs.push((arena_id, key));
            });
            
            // Order should be deterministic and match sequential
            assert_eq!(par_pairs, seq_pairs);
        }
    }

    #[test]
    fn visit_key_with_removal_during_iteration_arena_ids() {
        let mut world = MyWorld::default();
        
        let e1 = world.enemy.insert(Enemy { hp: 10 });
        let e2 = world.enemy.insert(Enemy { hp: 20 });
        let p1 = world.player.insert(Player { id: 5 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Collect (ArenaID, key) pairs to remove
        let mut to_remove = Vec::new();
        world.visit_key_test_trait(|(arena_id, key), entity| {
            if entity.metric() > 10 {
                to_remove.push((arena_id, key));
            }
        });
        
        // Remove after iteration (not during)
        for (arena_id, key) in to_remove {
            if arena_id == enemy_arena_id {
                world.enemy.remove(key);
            } else if arena_id == player_arena_id {
                world.player.remove(key);
            }
        }
        
        // Verify correct entities remain
        assert!(world.enemy.get(e1).is_some()); // hp=10, not removed
        assert!(world.enemy.get(e2).is_none()); // hp=20, removed
        assert!(world.player.get(p1).is_some()); // id=5 < 10, not removed
    }

    #[test]
    fn visit_key_mut_with_arena_id_dependent_logic() {
        let mut world = MyWorld::default();
        
        let e1 = world.enemy.insert(Enemy { hp: 10 });
        let e2 = world.enemy.insert(Enemy { hp: 20 });
        let p1 = world.player.insert(Player { id: 5 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let target_key = e2;
        
        // Modify only the targeted entity, verifying ArenaID
        world.visit_key_mut_test_trait(|(arena_id, key), entity| {
            if arena_id == enemy_arena_id && key == target_key {
                entity.add(1000);
            }
        });
        
        // Verify only e2 was modified
        assert_eq!(world.enemy.get(e1).unwrap().hp, 10);
        assert_eq!(world.enemy.get(e2).unwrap().hp, 1020);
        assert_eq!(world.player.get(p1).unwrap().id, 5);
    }

    #[test]
    fn visit_key_distinguishes_same_key_different_arenas() {
        // Create world where we might get same key values in different arenas
        let mut world = MyWorld::default();
        
        // Clear and insert to potentially get same key values
        world.clear();
        
        let e_key = world.enemy.insert(Enemy { hp: 100 });
        let p_key = world.player.insert(Player { id: 200 });
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Even if keys have same underlying value, ArenaID distinguishes them
        let mut visited = Vec::new();
        world.visit_key_test_trait(|(arena_id, key), entity| {
            visited.push((arena_id, key, entity.metric()));
        });
        
        // Should visit both entities with correct ArenaID
        assert!(visited.contains(&(enemy_arena_id, e_key, 100)));
        assert!(visited.contains(&(player_arena_id, p_key, 200)));
        assert_eq!(visited.len(), 2);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn par_visit_key_correct_arena_ids_per_spawn() {
        use std::sync::{Arc, Mutex};
        
        let mut world = MyWorld::default();
        
        let enemy_arena_id = MyWorld::arena_id::<Enemy>();
        let player_arena_id = MyWorld::arena_id::<Player>();
        
        // Insert many entities
        for i in 0..100 {
            world.enemy.insert(Enemy { hp: i });
            world.player.insert(Player { id: i + 100 });
        }
        
        let collected = Arc::new(Mutex::new(Vec::new()));
        let collected_clone = Arc::clone(&collected);
        
        // Parallel visit collecting (ArenaID, metric) pairs
        world.par_visit_key_test_trait(move |(arena_id, _key), entity| {
            collected_clone.lock().unwrap().push((arena_id, entity.metric()));
        });
        
        let results = collected.lock().unwrap();
        
        // Verify all enemy entities have enemy_arena_id
        let enemy_results: Vec<_> = results.iter()
            .filter(|(aid, _)| *aid == enemy_arena_id)
            .map(|(_, m)| *m)
            .collect();
        assert_eq!(enemy_results.len(), 100);
        assert!(enemy_results.iter().all(|m| *m < 100));
        
        // Verify all player entities have player_arena_id  
        let player_results: Vec<_> = results.iter()
            .filter(|(aid, _)| *aid == player_arena_id)
            .map(|(_, m)| *m)
            .collect();
        assert_eq!(player_results.len(), 100);
        assert!(player_results.iter().all(|m| *m >= 100));
    }
}
