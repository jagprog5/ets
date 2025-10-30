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

    fn pmv_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(&mut dyn $trait_name) + Send + Sync;

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

    default fn pmv_if_applicable<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(&mut dyn $trait_name) + Send + Sync {}

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

    fn pmv_if_applicable<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(&mut dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefMutIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_mut_slice()
            .par_iter_mut()
            .for_each(|entity| handler(entity));
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
                    $crate::__world_define_visitors_common!(@use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// mutably visit all entities that implement the trait
                #[allow(unused)]
                pub fn [<visit_mut_ $trait_name:snake>]<F>(&mut self, mut handler: F)
                where
                    F: FnMut(&mut dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@m_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
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
            )*
        }
    };

    (@use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::v_if_applicable(
                    &$self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@m_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<$struct_name VisitIf $trait_name>]<$entity>>::mv_if_applicable(
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
                    $crate::__world_define_visitors!(@p_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
                }

                /// in parallel, mutably visit all entities that implement the trait
                #[allow(unused)]
                pub fn [<par_visit_mut_ $trait_name:snake>]<F>(&mut self, handler: F)
                where
                    F: Fn(&mut dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@p_m_use_entity_tuple $struct_name $trait_name $entity_tuple self handler);
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

    (@p_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
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

    (@p_m_use_entity_tuple $struct_name:ident $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            use $crate::rayon::scope;
            scope(|s| {
                $(
                    if <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::ACTIVE {
                        s.spawn(|_| {
                            <() as [<$struct_name ParVisitIf $trait_name>]<$entity>>::pmv_if_applicable(
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

// design decisions:
// - no overlapping impl query. like "has trait X and not trait Y". this would
//   go into the unsoundness territory of specialization, and it can be done
//   already by simply defining a new trait which does exactly that
// - no key-value visitors. if this is needed, then on insertion the object can
//   store its own key
//   (immutable gather diff first, then apply later)
// - no generic type erased visitor, that would be too slow. if needed, define a
//   trait that all entities implement

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
        let e1 = world.enemy.insert(Enemy { hp: 10 });
        let e2 = world.enemy.insert(Enemy { hp: 20 });
        let p1 = world.player.insert(Player { id: 5 });

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

        // Verify order and values restored
        assert_eq!(world.enemy.get(e1).unwrap().hp, 10);
        assert_eq!(world.enemy.get(e2).unwrap().hp, 20);
        assert_eq!(world.player.get(p1).unwrap().id, 5);
    }

    #[test]
    fn clear_trait_specific_leaves_others() {
        let mut world = AnotherWorld::default();

        world.enemy.insert(Enemy { hp: 10 });
        world.player.insert(Player { id: 1 });
        world.non_trait.insert(NonTrait { val: 99 });

        // Clear only SecondTestTrait implementers (Player only)
        world.clear_second_test_trait();

        assert_eq!(world.player.len(), 0);
        assert_eq!(world.enemy.len(), 1); // Enemy doesn't implement SecondTestTrait
        assert_eq!(world.non_trait.len(), 1);
    }

    #[test]
    fn len_trait_specific_counts_only_implementers() {
        let mut world = AnotherWorld::default();

        world.enemy.insert(Enemy { hp: 10 });
        world.enemy.insert(Enemy { hp: 20 });
        world.player.insert(Player { id: 1 });
        world.non_trait.insert(NonTrait { val: 99 });

        // TestTrait: Enemy + Player (3 total)
        assert_eq!(world.len_test_trait(), 3);

        // SecondTestTrait: Player only (1 total)
        assert_eq!(world.len_second_test_trait(), 1);

        // Total world: all entities
        assert_eq!(world.len(), 4);
    }

    #[test]
    #[should_panic(expected = "not registered")]
    fn arena_access_panics_on_unregistered_type() {
        let world = MyWorld::default();
        // NonTrait is not in MyWorld
        let _ = world.arena::<NonTrait>();
    }

    #[test]
    #[should_panic(expected = "not registered")]
    fn arena_mut_access_panics_on_unregistered_type() {
        let mut world = MyWorld::default();
        let _ = world.arena_mut::<NonTrait>();
    }

    #[test]
    #[should_panic(expected = "not registered")]
    fn arena_id_panics_on_unregistered_type() {
        let _ = MyWorld::arena_id::<NonTrait>();
    }

    #[test]
    #[should_panic(expected = "No arena for type id")]
    fn get_panics_on_invalid_arena_id() {
        let world = MyWorld::default();
        // Create invalid arena ID
        let bad_id = MyWorldArenaID(999);
        let _ = world.any_arena(bad_id);
    }

    #[test]
    fn removed_keys_return_none() {
        let mut world = MyWorld::default();

        let player_id = MyWorld::arena_id::<Player>();
        let key = world.player.insert(Player { id: 42 });

        // Key is valid
        assert!(world.any_arena(player_id).get(key).is_some());

        // Remove the entity
        world.player.remove(key);

        // Key is now invalid
        assert!(world.any_arena(player_id).get(key).is_none());
    }

    #[test]
    fn downcast_to_wrong_type_returns_none() {
        let mut world = MyWorld::default();
        let player_id = MyWorld::arena_id::<Player>();
        let key = world.player.insert(Player { id: 42 });

        let entity = world.any_arena(player_id).get(key).unwrap();

        // Downcast to correct type succeeds
        assert!(entity.downcast_ref::<Player>().is_some());
        assert_eq!(entity.downcast_ref::<Player>().unwrap().id, 42);

        // Downcast to wrong type returns None
        assert!(entity.downcast_ref::<Enemy>().is_none());
        assert!(entity.downcast_ref::<NonTrait>().is_none());
    }

    #[test]
    fn downcast_mut_to_wrong_type_returns_none() {
        let mut world = MyWorld::default();
        let enemy_id = MyWorld::arena_id::<Enemy>();
        let key = world.enemy.insert(Enemy { hp: 100 });

        let entity = world.any_arena_mut(enemy_id).get_mut(key).unwrap();

        // Downcast to wrong type returns None
        assert!(entity.downcast_mut::<Player>().is_none());

        // Downcast to correct type succeeds
        assert!(entity.downcast_mut::<Enemy>().is_some());
        entity.downcast_mut::<Enemy>().unwrap().hp += 50;

        // Verify mutation persisted
        assert_eq!(world.enemy.get(key).unwrap().hp, 150);
    }

    #[test]
    fn get_unchecked_with_valid_key() {
        let mut world = MyWorld::default();
        let player_id = MyWorld::arena_id::<Player>();
        let key = world.player.insert(Player { id: 99 });

        unsafe {
            let entity = world.any_arena(player_id).get_unchecked(key);
            let player = entity.downcast_ref::<Player>().unwrap();
            assert_eq!(player.id, 99);
        }
    }

    #[test]
    fn get_unchecked_mut_with_valid_key() {
        let mut world = MyWorld::default();
        let enemy_id = MyWorld::arena_id::<Enemy>();
        let key = world.enemy.insert(Enemy { hp: 50 });

        unsafe {
            let entity = world.any_arena_mut(enemy_id).get_unchecked_mut(key);
            let enemy = entity.downcast_mut::<Enemy>().unwrap();
            enemy.hp *= 2;
        }

        assert_eq!(world.enemy.get(key).unwrap().hp, 100);
    }

    #[cfg(all(feature = "serde"))]
    #[test]
    fn arena_id_serde_roundtrip() {
        let arena_id = MyWorld::arena_id::<Player>();

        // JSON round-trip
        let serialized = serde_json::to_string(&arena_id).unwrap();
        assert_eq!(serialized, "\"Player\"");
        let deserialized: MyWorldArenaID = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, arena_id);

        // Enemy arena ID
        let enemy_id = MyWorld::arena_id::<Enemy>();
        let serialized = serde_json::to_string(&enemy_id).unwrap();
        assert_eq!(serialized, "\"Enemy\"");
        let deserialized: MyWorldArenaID = serde_json::from_str(&serialized).unwrap();
        assert_eq!(deserialized, enemy_id);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn arena_id_deserialize_unknown_type_fails() {
        let result = serde_json::from_str::<MyWorldArenaID>("\"UnknownType\"");
        assert!(result.is_err());
        assert!(
            result
                .unwrap_err()
                .to_string()
                .contains("Unknown ArenaID string")
        );
    }

    #[cfg(all(feature = "serde", feature = "debug"))]
    #[test]
    fn world_deserialize_unknown_field_fails() {
        // Valid JSON structure but with an unknown entity type field
        let json = r#"{"UnknownEntity":[]}"#;
        let result = serde_json::from_str::<MyWorld>(json);
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("Unknown field") && err_msg.contains("UnknownEntity"));
    }

    #[test]
    fn visitor_ordering_matches_insertion_order() {
        let mut world = MyWorld::default();

        // Insert in specific order
        world.enemy.insert(Enemy { hp: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 3 });
        world.player.insert(Player { id: 4 });
        world.enemy.insert(Enemy { hp: 5 });

        // Visit should iterate enemies first (in insertion order), then players
        let mut metrics = Vec::new();
        world.visit_test_trait(|t| metrics.push(t.metric()));

        // Enemies: 1, 3, 5; Players: 2, 4
        assert_eq!(metrics, vec![1, 3, 5, 2, 4]);
    }

    #[test]
    fn single_entity_type_world() {
        // World with only one entity type
        world!(SingleWorld, Player; TestTrait);

        let mut world = SingleWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });

        assert_eq!(world.len(), 2);
        assert_eq!(world.len_test_trait(), 2);

        let mut sum = 0;
        world.visit_test_trait(|t| sum += t.metric());
        assert_eq!(sum, 3);
    }

    #[test]
    fn no_trait_world() {
        // World with entity types but no queryable traits
        world!(NoTraitWorld, Player, Enemy;);

        let mut world = NoTraitWorld::default();
        world.player.insert(Player { id: 1 });
        world.enemy.insert(Enemy { hp: 10 });

        assert_eq!(world.len(), 2);
        assert_eq!(world.arena::<Player>().len(), 1);
        assert_eq!(world.arena::<Enemy>().len(), 1);
    }

    #[test]
    fn visitor_with_side_effects() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 5 });

        // Mutable visitor that increments all values
        world.visit_mut_test_trait(|t| t.add(10));

        // Verify side effects persisted
        let mut total = 0;
        world.visit_test_trait(|t| total += t.metric());
        assert_eq!(total, (1 + 10) + (2 + 10) + (5 + 10)); // 41
    }

    #[test]
    fn diff_apply_with_transformation() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 5 });
        world.enemy.insert(Enemy { hp: 10 });
        world.enemy.insert(Enemy { hp: 15 });

        // Create diff: square all metrics
        let diff = world.diff_test_trait(|t| {
            let m = t.metric();
            m * m
        });
        assert_eq!(diff, vec![100, 225, 25]); // hp: 10^2, 15^2, id: 5^2

        // Apply: set metric to diff value (via add(diff - current))
        world.diff_apply_test_trait(diff.clone(), |t, squared| {
            let current = t.metric();
            t.add(squared - current);
        });

        // Verify transformation applied
        let final_metrics: Vec<i32> = world.diff_test_trait(|t| t.metric());
        assert_eq!(final_metrics, vec![100, 225, 25]);
    }

    #[test]
    fn many_entities_iteration() {
        let mut world = MyWorld::default();

        // Insert many entities
        for i in 0..100 {
            world.player.insert(Player { id: i });
            world.enemy.insert(Enemy { hp: i * 2 });
        }

        assert_eq!(world.len(), 200);
        assert_eq!(world.len_test_trait(), 200);

        // Verify all visited exactly once
        let mut count = 0;
        world.visit_test_trait(|_| count += 1);
        assert_eq!(count, 200);

        // Verify retain works on large set
        world.retain_test_trait(|t| t.metric() < 50);
        assert!(world.len_test_trait() < 200);
    }

    #[test]
    fn clear_individual_arenas_vs_world_clear() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.enemy.insert(Enemy { hp: 10 });

        // Clear just player arena
        world.player.clear();
        assert_eq!(world.player.len(), 0);
        assert_eq!(world.enemy.len(), 1);
        assert_eq!(world.len(), 1);

        // Add back
        world.player.insert(Player { id: 2 });
        assert_eq!(world.len(), 2);

        // Clear entire world
        world.clear();
        assert_eq!(world.len(), 0);
        assert_eq!(world.player.len(), 0);
        assert_eq!(world.enemy.len(), 0);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn parallel_visitor_ordering_deterministic() {
        let mut world = MyWorld::default();

        for i in 0..50 {
            world.player.insert(Player { id: i });
            world.enemy.insert(Enemy { hp: i * 2 });
        }

        // Parallel diff should produce same result as sequential
        let pdiff = world.par_diff_test_trait(|t| t.metric());
        let sdiff = world.diff_test_trait(|t| t.metric());

        assert_eq!(pdiff, sdiff);
    }

    #[cfg(all(feature = "serde"))]
    #[test]
    fn serde_preserves_slot_keys() {
        let mut world = MyWorld::default();

        let p_key = world.player.insert(Player { id: 42 });
        let e_key = world.enemy.insert(Enemy { hp: 100 });

        // Serialize
        let serialized = serde_json::to_string(&world).unwrap();

        // Deserialize
        let mut de_world: MyWorld = serde_json::from_str(&serialized).unwrap();

        // Original keys should still work in deserialized world
        assert_eq!(de_world.player.get(p_key).unwrap().id, 42);
        assert_eq!(de_world.enemy.get(e_key).unwrap().hp, 100);

        // Can still insert new entities
        de_world.player.insert(Player { id: 7 });
        assert_eq!(de_world.player.len(), 2);
    }

    #[cfg(all(feature = "serde"))]
    #[test]
    fn serde_with_mixed_arenas() {
        // Test world where some arenas are empty
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        // enemy arena is empty

        let serialized = serde_json::to_string(&world).unwrap();
        let de_world: MyWorld = serde_json::from_str(&serialized).unwrap();

        assert_eq!(de_world.player.len(), 1);
        assert_eq!(de_world.enemy.len(), 0);
    }

    #[test]
    fn nested_visitor_calls() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 10 });

        // Nested immutable visitors: outer visitor calls inner visitor
        let mut outer_count = 0;
        world.visit_test_trait(|outer_entity| {
            outer_count += 1;
            let outer_metric = outer_entity.metric();

            // Inner visitor: count entities with metric > outer_metric
            let mut inner_count = 0;
            world.visit_test_trait(|inner_entity| {
                if inner_entity.metric() > outer_metric {
                    inner_count += 1;
                }
            });

            // Just verify it doesn't panic and runs
            let _ = inner_count;
        });

        assert_eq!(outer_count, 3); // Visited all entities
    }

    #[test]
    fn diff_apply_with_removed_entities() {
        let mut world = MyWorld::default();
        let p1 = world.player.insert(Player { id: 1 });
        world.player.insert(Player { id: 2 });
        world.enemy.insert(Enemy { hp: 10 });

        // Create diff
        let diff = world.diff_test_trait(|t| t.metric() * 2);
        assert_eq!(diff.len(), 3);

        // Remove an entity
        world.player.remove(p1);

        // Applying mismatched diff leads to incorrect behavior
        // (applying diff[2] to wrong entity) - this documents the footgun
        assert_ne!(world.len_test_trait(), diff.len());
    }

    #[test]
    fn arena_id_independent_across_worlds() {
        // Define a second world with same entity types but different order
        world!(WorldA, Player, Enemy; TestTrait);
        world!(WorldB, Enemy, Player; TestTrait);

        let id_a_player = WorldA::arena_id::<Player>();
        let id_a_enemy = WorldA::arena_id::<Enemy>();
        let id_b_player = WorldB::arena_id::<Player>();
        let id_b_enemy = WorldB::arena_id::<Enemy>();

        // WorldA: Player=0, Enemy=1 (declaration order)
        assert_eq!(id_a_player.0, 0);
        assert_eq!(id_a_enemy.0, 1);

        // WorldB: Enemy=0, Player=1 (reverse order)
        assert_eq!(id_b_enemy.0, 0);
        assert_eq!(id_b_player.0, 1);

        // Same entity type has different ArenaIDs in different worlds
        assert_ne!(id_a_player.0, id_b_player.0);
        assert_ne!(id_a_enemy.0, id_b_enemy.0);

        // Type equality still works within each world
        assert_eq!(WorldA::arena_id::<Player>(), id_a_player);
        assert_eq!(WorldB::arena_id::<Player>(), id_b_player);
    }

    #[test]
    fn diff_mut_modifies_during_diff_creation() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 5 });
        world.player.insert(Player { id: 10 });
        world.enemy.insert(Enemy { hp: 20 });
        world.enemy.insert(Enemy { hp: 30 });

        // diff_mut: capture current metric and increment it
        let diff = world.diff_mut_test_trait(|t| {
            let current = t.metric();
            t.add(1); // modify during diff creation
            current // return original value
        });

        // Diff should contain original values
        assert_eq!(diff, vec![20, 30, 5, 10]);

        // Entities should be incremented
        let mut metrics = Vec::new();
        world.visit_test_trait(|t| metrics.push(t.metric()));
        assert_eq!(metrics, vec![21, 31, 6, 11]);

        // Apply diff to restore original values
        world.diff_apply_test_trait(diff, |t, original| {
            let current = t.metric();
            t.add(original - current);
        });

        // Verify restoration
        let mut final_metrics = Vec::new();
        world.visit_test_trait(|t| final_metrics.push(t.metric()));
        assert_eq!(final_metrics, vec![20, 30, 5, 10]);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn par_diff_mut_modifies_during_diff_creation() {
        let mut world = MyWorld::default();
        world.player.insert(Player { id: 5 });
        world.player.insert(Player { id: 10 });
        world.enemy.insert(Enemy { hp: 20 });
        world.enemy.insert(Enemy { hp: 30 });

        // par_diff_mut: capture current metric and increment it
        let diff = world.par_diff_mut_test_trait(|t| {
            let current = t.metric();
            t.add(1);
            current
        });

        // Diff should contain original values (order preserved)
        assert_eq!(diff, vec![20, 30, 5, 10]);

        // Entities should be incremented
        let mut metrics = Vec::new();
        world.visit_test_trait(|t| metrics.push(t.metric()));
        assert_eq!(metrics, vec![21, 31, 6, 11]);
    }
}
