// specialization feature is incomplete, but is being used here in a very
// limited but required capacity. it is only used to check if a type implements
// a trait - there is no overlapping impl weirdness that is relied on
#![allow(incomplete_features)]
#![feature(specialization)]
#![forbid(unsafe_code)]

// macro is expanded at call site - makes it available to the lib user
#[cfg(feature = "serde")]
#[doc(hidden)]
pub use once_cell;
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

// macro is expanded at call site - must be pub
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

#[doc(hidden)]
pub trait ArenaCast<T> {
    fn cast(&self) -> &slotmap::DenseSlotMap<slotmap::DefaultKey, T>;
    fn cast_mut(&mut self) -> &mut slotmap::DenseSlotMap<slotmap::DefaultKey, T>;
}

// default: panic for mismatched types
impl<A, B> ArenaCast<B> for slotmap::DenseSlotMap<slotmap::DefaultKey, A> {
    default fn cast(&self) -> &slotmap::DenseSlotMap<slotmap::DefaultKey, B> {
        panic!( // never, gated at compile time
            "Arena type mismatch: {} cannot be cast to {}",
            std::any::type_name::<A>(),
            std::any::type_name::<B>()
        );
    }

    default fn cast_mut(&mut self) -> &mut slotmap::DenseSlotMap<slotmap::DefaultKey, B> {
        panic!( // never, gated at compile time
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

// type erased view to arena of some type
pub trait ErasedArena {
    fn get_any(&self, key: slotmap::DefaultKey) -> Option<&dyn std::any::Any>;
    fn get_any_mut(&mut self, key: slotmap::DefaultKey) -> Option<&mut dyn std::any::Any>;
}

impl<T: 'static> ErasedArena for slotmap::DenseSlotMap<slotmap::DefaultKey, T> {
    fn get_any(&self, key: slotmap::DefaultKey) -> Option<&dyn std::any::Any> {
        self.get(key).map(|e| e as &dyn std::any::Any)
    }

    fn get_any_mut(&mut self, key: slotmap::DefaultKey) -> Option<&mut dyn std::any::Any> {
        self.get_mut(key).map(|e| e as &mut dyn std::any::Any)
    }
}

#[macro_export]
#[cfg(not(feature = "rayon"))]
macro_rules! __world_define_rayon_trait_helpers {
    ($( $trait_name:ident ),*) => {};
}

#[macro_export]
#[cfg(feature = "rayon")]
macro_rules! __world_define_rayon_trait_helpers {
    ($( $trait_name:ident ),*) => {
        $crate::paste::paste! {

$(
trait [<ParVisitIf $trait_name>]<T> {
    fn par_visit_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(&dyn $trait_name) + Send + Sync;

    fn par_visit_if_applicable_mut<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where
        F: Fn(&mut dyn $trait_name) + Send + Sync;
}

impl<T> [<ParVisitIf $trait_name>]<T> for () {
    default fn par_visit_if_applicable<F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(&dyn $trait_name) + Send + Sync {}

    default fn par_visit_if_applicable_mut<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: &F)
    where F: Fn(&mut dyn $trait_name) + Send + Sync {}
}

impl<T> [<ParVisitIf $trait_name>]<T> for ()
where
    T: $trait_name + Send + Sync,
{
    fn par_visit_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(&dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_slice()
            .par_iter()
            .for_each(|entity| handler(entity as &dyn $trait_name));
    }

    fn par_visit_if_applicable_mut<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: &F)
    where F: Fn(&mut dyn $trait_name) + Send + Sync
    {
        use $crate::rayon::iter::IntoParallelRefMutIterator;
        use $crate::rayon::iter::ParallelIterator;
        arena
            .values_as_mut_slice()
            .par_iter_mut()
            .for_each(|entity| handler(entity as &mut dyn $trait_name));
    }
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
    (@pass_entity_tuple $($trait_name:ident),* @ $entity_tuple:tt) => {
        $crate::paste::paste! {
            $(
                #[allow(unused)]
                pub fn [<visit_ $trait_name:snake>]<F>(&self, mut handler: F)
                where
                    F: FnMut(&dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@use_entity_tuple $trait_name $entity_tuple self handler);
                }

                #[allow(unused)]
                pub fn [<visit_mut_ $trait_name:snake>]<F>(&mut self, mut handler: F)
                where
                    F: FnMut(&mut dyn $trait_name)
                {
                    $crate::__world_define_visitors_common!(@use_entity_tuple_mut $trait_name $entity_tuple self handler);
                }
            )*
        }
    };

    (@use_entity_tuple $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<VisitIf $trait_name>]<$entity>>::visit_if_applicable(
                    &$self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@use_entity_tuple_mut $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            $(
                <() as [<VisitIf $trait_name>]<$entity>>::visit_if_applicable_mut(
                    &mut $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };
}

#[macro_export]
#[cfg(not(feature = "rayon"))]
macro_rules! __world_define_visitors {
    // Non-rayon version simply forwards to the common macro
    (@pass_entity_tuple $($trait_name:ident),* @ $entity_tuple:tt) => {
        $crate::__world_define_visitors_common!(@pass_entity_tuple $($trait_name),* @ $entity_tuple);
    };
}

#[macro_export]
#[cfg(feature = "rayon")]
macro_rules! __world_define_visitors {
    // https://stackoverflow.com/a/37754096/15534181
    //
    // generate visit_* functions per trait
    (@pass_entity_tuple $($trait_name:ident),* @ $entity_tuple:tt) => {
        // non-parallel visit functions
        $crate::__world_define_visitors_common!(@pass_entity_tuple $($trait_name),* @ $entity_tuple);

        // parallel visit functions (added only when rayon feature enabled)
        $crate::paste::paste! {
            $(
                #[allow(unused)]
                pub fn [<par_visit_ $trait_name:snake>]<F>(&self, handler: F)
                where
                    F: Fn(&dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@use_entity_tuple_par $trait_name $entity_tuple self handler);
                }

                #[allow(unused)]
                pub fn [<par_visit_mut_ $trait_name:snake>]<F>(&mut self, handler: F)
                where
                    F: Fn(&mut dyn $trait_name) + Send + Sync
                {
                    $crate::__world_define_visitors!(@use_entity_tuple_par_mut $trait_name $entity_tuple self handler);
                }
            )*
        }
    };

    (@use_entity_tuple_par $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            use $crate::rayon::scope;
            scope(|s| {
                $(
                    let arena_ref = &$self_ident.[<$entity:snake>];
                    s.spawn(|_| {
                        <() as [<ParVisitIf $trait_name>]<$entity>>::par_visit_if_applicable(
                            arena_ref,
                            &$handler_ident,
                        );
                    });
                )*

            });
        }
    };

    (@use_entity_tuple_par_mut $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        $crate::paste::paste! {
            use $crate::rayon::scope;
            scope(|s| {
                $(
                    let arena_ref = &mut $self_ident.[<$entity:snake>];
                    let handler_ref = &$handler_ident;
                    s.spawn(|_| {
                        <() as [<ParVisitIf $trait_name>]<$entity>>::par_visit_if_applicable_mut(
                            arena_ref,
                            handler_ref,
                        );
                    });
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
    ($struct_name:ident, $( $entity:ty ),*) => {};
}

#[macro_export]
#[cfg(feature = "serde")]
macro_rules! __world_serde_support {
    ($struct_name:ident, $( $entity:ty ),*) => {
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

// mut be 'static, and can't do this at compile time :(
static [<$struct_name:upper _DESERIALIZE_FIELDS>]: $crate::once_cell::sync::Lazy<Vec<&'static str>> =
    $crate::once_cell::sync::Lazy::new(|| {
        vec![
            $(
                if <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as $crate::SerdeArena<'static>>::ACTIVE {
                    Some(stringify!($entity))
                } else {
                    None
                }
            ),*
        ]
        .into_iter()
        .flatten() // converts Option<&str> -> only keep Some
        .collect()
    });


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

/// select type erased arena at runtime
// instead of typeid, which is not stable between rust version. e.g.
// serialization? this id is determined by the order stated by the user when
// creating the world
//
// this is declared per world for safety - if serde is enabled and the entity
// order changes, it still goes to the correct entity
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct [<$struct_name ArenaID>](usize);

$crate::__world_define_struct!($struct_name, $($entity),*);

$(
    trait [<VisitIf $trait_name>]<T> {
        fn visit_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(&dyn $trait_name);

        fn visit_if_applicable_mut<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(&mut dyn $trait_name);
    }

    // no-op for types not implementing the trait
    impl<T> [<VisitIf $trait_name>]<T> for () {
        default fn visit_if_applicable<F>(_arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(&dyn $trait_name) {}

        default fn visit_if_applicable_mut<F>(_arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(&mut dyn $trait_name) {}
    }

    impl<T: $trait_name> [<VisitIf $trait_name>]<T> for () {
        fn visit_if_applicable<F>(arena: &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(&dyn $trait_name)
        {
            arena
                .values_as_slice()
                .iter()
                .for_each(|entity| handler(entity as &dyn $trait_name));
        }

        fn visit_if_applicable_mut<F>(arena: &mut $crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(&mut dyn $trait_name)
        {
            arena
                .values_as_mut_slice()
                .iter_mut()
                .for_each(|entity| handler(entity as &mut dyn $trait_name));
        }
    }

)*

impl $struct_name {
    $crate::__world_define_visitors!(@pass_entity_tuple $($trait_name),* @ ($($entity),*));

    #[allow(unused)]
    pub fn arena<T>(&self) -> &$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, T> {
        use $crate::ArenaCast;
        $(
            if <T as $crate::IsType<$entity>>::VALUE {
                return <$crate::slotmap::DenseSlotMap<$crate::slotmap::DefaultKey, $entity> as ArenaCast<T>>::cast(&self.[<$entity:snake>]);
            }
        )*
        panic!("In call to {}::arena::<{}>(), {} not registered", stringify!($struct_name), std::any::type_name::<T>(), std::any::type_name::<T>());
    }

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

    #[allow(unused)]
    pub fn arena_erased(&self, id: [<$struct_name ArenaID>]) -> &dyn $crate::ErasedArena {
        match id.0 {
            $(
                i if i == Self::arena_id::<$entity>().0 => &self.[<$entity:snake>] as &dyn $crate::ErasedArena,
            )*
            _ => panic!("No arena for type id {}", id.0),
        }
    }

    #[allow(unused)]
    pub fn arena_erased_mut(&mut self, id: [<$struct_name ArenaID>]) -> &mut dyn $crate::ErasedArena {
        match id.0 {
            $(
                i if i == Self::arena_id::<$entity>().0 => &mut self.[<$entity:snake>] as &mut dyn $crate::ErasedArena,
            )*
            _ => panic!("No arena for type id {}", id.0),
        }
    }
}

$crate::__world_serde_support!($struct_name, $($entity),*);

$crate::__world_define_rayon_trait_helpers!($($trait_name),*);

        }
    };
}

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------
#[cfg(test)]
mod tests {
    #[derive(Debug)]
    #[cfg_attr(
        feature = "serde",
        derive(serde::Serialize, serde::Deserialize, bincode::Encode, bincode::Decode)
    )]
    struct Player {
        id: u32,
    }
    #[derive(Debug)]
    // each type individually can opt in to being serde
    #[cfg_attr(
        feature = "serde",
        derive(serde::Serialize, serde::Deserialize, bincode::Encode, bincode::Decode)
    )]
    struct Enemy {
        hp: u32,
    }

    pub trait TestTrait {
        fn do_something(&self);
    }
    impl TestTrait for Player {
        fn do_something(&self) {
            println!("Player {}", self.id)
        }
    }
    impl TestTrait for Enemy {
        fn do_something(&self) {
            println!("Enemy {}", self.hp)
        }
    }

    pub trait SecondTestTrait {
        fn do_something_else(&mut self);
    }
    impl SecondTestTrait for Player {
        fn do_something_else(&mut self) {
            println!("Player second trait")
        }
    }

    world!(MyWorld, Enemy, Player; TestTrait, SecondTestTrait);

    #[test]
    fn do_tests() {
        let mut world = MyWorld::default();
        // directly access arena member
        let player_id = world.player.insert(Player { id: 1 });
        // compile time type accessor of arena member
        world.arena_mut::<Enemy>().insert(Enemy { hp: 10 });

        // world.arena_mut::<usize>();

        // visit all arena with types that implement trait
        #[cfg(feature = "rayon")]
        world.par_visit_test_trait(|e| e.do_something());
        #[cfg(not(feature = "rayon"))]
        world.visit_test_trait(|e| e.do_something());

        #[cfg(feature = "rayon")]
        world.par_visit_mut_second_test_trait(|e| e.do_something_else());
        #[cfg(not(feature = "rayon"))]
        world.visit_mut_second_test_trait(|e| e.do_something_else());

        // runtime type accessor. unique id is tuple (arena_id, arena_key).
        // manage it however you decide!
        let arena_id = MyWorld::arena_id::<Player>();
        let arena = world.arena_erased(arena_id);
        // unwrap: I know that this is a player and that the reference is valid
        let player = arena
            .get_any(player_id)
            .unwrap()
            .downcast_ref::<Player>()
            .unwrap();
        player.do_something();

        #[cfg(all(feature = "serde", feature = "debug"))]
        {
            println!("testing serialization round trips");
            // round trip different formats
            let serialized = serde_json::to_string(&world).unwrap();
            let deserialized_json: MyWorld = serde_json::from_str(&serialized).unwrap();

            let serialized: Vec<u8> =
                bincode::serde::encode_to_vec(&world, bincode::config::standard()).unwrap();
            let (deserialized_bincode, _bytes_read): (MyWorld, usize) =
                bincode::serde::decode_from_slice(&serialized, bincode::config::standard())
                    .unwrap();

            assert_eq!(format!("{:?}", world), format!("{:?}", deserialized_json));
            assert_eq!(
                format!("{:?}", world),
                format!("{:?}", deserialized_bincode)
            );
        }
    }
}
