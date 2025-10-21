// specialization feature is incomplete, but is being used here in a very
// limited but required capacity
#![allow(incomplete_features)]
#![feature(specialization)]

#[allow(unused)]
trait IsType<T> {
    const VALUE: bool;
}

// default: false
impl<A, B> IsType<B> for A {
    default const VALUE: bool = false;
}

// specialize for equal types
impl<T> IsType<T> for T {
    const VALUE: bool = true;
}

/// select type erased arena at runtime
// instead of typeid, which is not stable between rust version. e.g.
// serialization? this id is determined by the order stated by the user when
// creating the world
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ArenaID(pub usize);

// --------------------------------------------------------------------
// Type-erased arena trait
// --------------------------------------------------------------------
pub trait ErasedArena {
    fn get_any(&self, key: slotmap::DefaultKey) -> Option<&dyn std::any::Any>;
    fn get_any_mut(&mut self, key: slotmap::DefaultKey) -> Option<&mut dyn std::any::Any>;
}

// Blanket impl for all typed arenas
// 'static required for Any
impl<T: 'static> ErasedArena for slotmap::DenseSlotMap::<slotmap::DefaultKey, T> {
    fn get_any(&self, key: slotmap::DefaultKey) -> Option<&dyn std::any::Any> {
        self.get(key).map(|e| e as &dyn std::any::Any)
    }

    fn get_any_mut(&mut self, key: slotmap::DefaultKey) -> Option<&mut dyn std::any::Any> {
        self.get_mut(key).map(|e| e as &mut dyn std::any::Any)
    }
}

// --------------------------------------------------------------------
// World macro
// --------------------------------------------------------------------
#[macro_export]
macro_rules! world {
    // https://stackoverflow.com/a/37754096/15534181
    //
    // generate visit_* functions per trait
    (@pass_entity_tuple $($trait_name:ident),* @ $entity_tuple:tt) => {
        paste::paste! {
            $(
                #[allow(unused)]
                pub fn [<visit_ $trait_name:snake>]<F>(&self, mut handler: F)
                where
                    F: FnMut(&dyn $trait_name)
                {
                    $crate::world!(@use_entity_tuple $trait_name $entity_tuple self handler);
                }

                #[allow(unused)]
                pub fn [<visit_mut_ $trait_name:snake>]<F>(&mut self, mut handler: F)
                where
                    F: FnMut(&mut dyn $trait_name)
                {
                    $crate::world!(@use_entity_tuple_mut $trait_name $entity_tuple self handler);
                }

                #[cfg(feature = "rayon")]
                #[allow(unused)]
                pub fn [<par_visit_ $trait_name:snake>]<F>(&self, handler: F)
                where
                    F: Fn(&dyn $trait_name) + Send + Sync
                {
                    use rayon::prelude::*;
                    use rayon::scope;
                    $crate::world!(@use_entity_tuple_par $trait_name $entity_tuple self handler);
                }

                #[cfg(feature = "rayon")]
                #[allow(unused)]
                pub fn [<par_visit_mut_ $trait_name:snake>]<F>(&mut self, handler: F)
                where
                    F: Fn(&mut dyn $trait_name) + Send + Sync
                {
                    $crate::world!(@use_entity_tuple_par_mut $trait_name $entity_tuple self handler);
                }
            )*
        }
    };

    (@use_entity_tuple $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        paste::paste! {
            $(
                <() as [<VisitIf $trait_name>]<$entity>>::visit_if_applicable(
                    &$self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@use_entity_tuple_mut $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        paste::paste! {
            $(
                <() as [<VisitIf $trait_name>]<$entity>>::visit_if_applicable_mut(
                    &mut $self_ident.[<$entity:snake>],
                    &mut $handler_ident,
                );
            )*
        }
    };

    (@use_entity_tuple_par $trait_name:ident ($( $entity:ty ),*) $self_ident:ident $handler_ident:ident) => {
        paste::paste! {
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
    paste::paste! {
        use rayon::scope;
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

    // main macro form: define struct + traits + impl
    (
        // the name of the struct being defined
        $struct_name:ident, // the types of entities which can exist in the world
        $( $entity:ty ),* $(,)? // optional trailing comma
        ;// semi colon separator between lists
        // the traits which are query-able over all types in the world
        $( $trait_name:ident ),* $(,)? // optional trailing comma
    ) => {
        paste::paste! {

#[derive(Default)]
#[allow(private_interfaces)] // member is pub even if underlying type isn't
pub struct $struct_name {
    $(
        pub [<$entity:snake>]: slotmap::DenseSlotMap::<slotmap::DefaultKey, $entity>,
    )*
}

$(
    trait [<VisitIf $trait_name>]<T> {
        fn visit_if_applicable<F>(arena: &slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(&dyn $trait_name);

        fn visit_if_applicable_mut<F>(arena: &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, handler: F)
        where
            F: FnMut(&mut dyn $trait_name);
    }

    // no-op for types not implementing the trait
    impl<T> [<VisitIf $trait_name>]<T> for () {
        default fn visit_if_applicable<F>(_arena: &slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(&dyn $trait_name) {}

        default fn visit_if_applicable_mut<F>(_arena: &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, _handler: F)
        where F: FnMut(&mut dyn $trait_name) {}
    }

    impl<T: $trait_name> [<VisitIf $trait_name>]<T> for () {
        fn visit_if_applicable<F>(arena: &slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(&dyn $trait_name)
        {
            arena
                .values_as_slice()
                .iter()
                .for_each(|entity| handler(entity as &dyn $trait_name));
        }

        fn visit_if_applicable_mut<F>(arena: &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, mut handler: F)
        where F: FnMut(&mut dyn $trait_name)
        {
            arena
                .values_as_mut_slice()
                .iter_mut()
                .for_each(|entity| handler(entity as &mut dyn $trait_name));
        }
    }

    // =========================================================================

    // Parallel version — requires Send/Sync, new par_* functions
    #[cfg(feature = "rayon")]
    trait [<ParVisitIf $trait_name>]<T> {
        fn par_visit_if_applicable<F>(arena: &slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, handler: &F)
        where
            F: Fn(&dyn $trait_name) + Send + Sync;

        fn par_visit_if_applicable_mut<F>(arena: &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, handler: &F)
        where
            F: Fn(&mut dyn $trait_name) + Send + Sync;
    }

    #[cfg(feature = "rayon")]
    impl<T> [<ParVisitIf $trait_name>]<T> for () {
        default fn par_visit_if_applicable<F>(_arena: &slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, _handler: &F)
        where F: Fn(&dyn $trait_name) + Send + Sync {}

        default fn par_visit_if_applicable_mut<F>(_arena: &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, _handler: &F)
        where F: Fn(&mut dyn $trait_name) + Send + Sync {}
    }

    #[cfg(feature = "rayon")]
    impl<T> [<ParVisitIf $trait_name>]<T> for ()
    where
        T: $trait_name + Send + Sync,
    {
        fn par_visit_if_applicable<F>(arena: &slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, handler: &F)
        where F: Fn(&dyn $trait_name) + Send + Sync
        {
            use rayon::iter::IntoParallelRefIterator;
            use rayon::iter::ParallelIterator;

            arena
                .values_as_slice()
                .par_iter()
                .for_each(|entity| handler(entity as &dyn $trait_name));
        }

        fn par_visit_if_applicable_mut<F>(arena: &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>, handler: &F)
        where F: Fn(&mut dyn $trait_name) + Send + Sync
        {
            use rayon::iter::IntoParallelRefMutIterator;
            use rayon::iter::ParallelIterator;
            arena
                .values_as_mut_slice()
                .par_iter_mut()
                .for_each(|entity| handler(entity as &mut dyn $trait_name));
        }
    }
)*

impl $struct_name {
    $crate::world!(@pass_entity_tuple $($trait_name),* @ ($($entity),*));

    #[allow(unused)]
    pub fn arena<T>(&self) -> &slotmap::DenseSlotMap::<slotmap::DefaultKey, T> {
        $(
            if <T as crate::IsType<$entity>>::VALUE {
                let ptr = &self.[<$entity:snake>] as *const slotmap::DenseSlotMap::<slotmap::DefaultKey, $entity>;
                return unsafe { &*(ptr as *const slotmap::DenseSlotMap::<slotmap::DefaultKey, T>) };
            }
        )*
        panic!("No arena for type {}", std::any::type_name::<T>());
    }

    #[allow(unused)]
    pub fn arena_mut<T>(&mut self) -> &mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T> {
        $(
            if <T as crate::IsType<$entity>>::VALUE {
                let ptr = &mut self.[<$entity:snake>] as *mut slotmap::DenseSlotMap::<slotmap::DefaultKey, $entity>;
                return unsafe { &mut *(ptr as *mut slotmap::DenseSlotMap::<slotmap::DefaultKey, T>) };
            }
        )*
        panic!("No arena for type {}", std::any::type_name::<T>());
    }

    pub fn arena_id<T>() -> crate::ArenaID {
        let mut i = 0usize;
        $(
            if <T as crate::IsType<$entity>>::VALUE {
                return crate::ArenaID(i);
            }
            i += 1;
        )*
        panic!("Type not registered in world! macro");
    }

    #[allow(unused)]
    pub fn arena_erased(&self, id: crate::ArenaID) -> &dyn crate::ErasedArena {
        match id.0 {
            $(
                i if i == Self::arena_id::<$entity>().0 => &self.[<$entity:snake>] as &dyn crate::ErasedArena,
            )*
            _ => panic!("No arena for type id {}", id.0),
        }
    }

    #[allow(unused)]
    pub fn arena_erased_mut(&mut self, id: crate::ArenaID) -> &mut dyn crate::ErasedArena {
        match id.0 {
            $(
                i if i == Self::arena_id::<$entity>().0 => &mut self.[<$entity:snake>] as &mut dyn crate::ErasedArena,
            )*
            _ => panic!("No arena for type id {}", id.0),
        }
    }
}

        }
    };
}

// ---------------------------------------------------------------
// Tests
// ---------------------------------------------------------------
#[cfg(test)]
mod tests {
    #[derive(Debug)]
    struct Player { id: u32 }
    #[derive(Debug)]
    struct Enemy { hp: u32 }

    pub trait TestTrait { fn do_something(&self); }
    impl TestTrait for Player { fn do_something(&self) { println!("Player {}", self.id) } }
    impl TestTrait for Enemy { fn do_something(&self) { println!("Enemy {}", self.hp) } }

    pub trait SecondTestTrait { fn do_something_else(&mut self); }
    impl SecondTestTrait for Player { fn do_something_else(&mut self) { println!("Player second trait") } }

    world!(MyWorld, Player, Enemy; TestTrait, SecondTestTrait);

    #[test]
    fn test_visit_traits() {
        let mut world = MyWorld::default();

        // directly access arena member
        let player_id = world.player.insert(Player { id: 1 });

        // compile time type accessor of arena member
        world.arena_mut::<Enemy>().insert(Enemy { hp: 10 });

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

        let player = arena.get_any(player_id).unwrap().downcast_ref::<Player>().unwrap();
        player.do_something();
    }
}
