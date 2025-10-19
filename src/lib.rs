// specialization feature is incomplete, but is being used here in a very
// limited but required capacity
#![allow(incomplete_features)]
#![feature(specialization)]

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
                    world!(@use_entity_tuple $trait_name $entity_tuple self handler);
                }

                #[allow(unused)]
                pub fn [<visit_mut_ $trait_name:snake>]<F>(&mut self, mut handler: F)
                where
                    F: FnMut(&mut dyn $trait_name)
                {
                    world!(@use_entity_tuple_mut $trait_name $entity_tuple self handler);
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
        pub [<$entity:snake>]: generational_arena::Arena<$entity>,
    )*
}

$(
    trait [<VisitIf $trait_name>]<T> {
        fn visit_if_applicable<F>(arena: &generational_arena::Arena<T>, handler: F)
        where
            F: FnMut(&dyn $trait_name);

        fn visit_if_applicable_mut<F>(arena: &mut generational_arena::Arena<T>, handler: F)
        where
            F: FnMut(&mut dyn $trait_name);
    }

    // no-op for types not implementing the trait
    impl<T> [<VisitIf $trait_name>]<T> for () {
        default fn visit_if_applicable<F>(_arena: &generational_arena::Arena<T>, _handler: F)
        where
            F: FnMut(&dyn $trait_name),
        {}

        default fn visit_if_applicable_mut<F>(_arena: &mut generational_arena::Arena<T>, _handler: F)
        where
            F: FnMut(&mut dyn $trait_name),
        {}
    }

    impl<T: $trait_name> [<VisitIf $trait_name>]<T> for () {
        fn visit_if_applicable<F>(arena: &generational_arena::Arena<T>, mut handler: F)
        where
            F: FnMut(&dyn $trait_name),
        {
            for (_idx, entity) in arena.iter() {
                handler(entity as &dyn $trait_name);
            }
        }

        fn visit_if_applicable_mut<F>(arena: &mut generational_arena::Arena<T>, mut handler: F)
        where
            F: FnMut(&mut dyn $trait_name),
        {
            for (_idx, entity) in arena.iter_mut() {
                handler(entity as &mut dyn $trait_name);
            }
        }
    }
)*

impl $struct_name {
    $crate::world!(@pass_entity_tuple $($trait_name),* @ ($($entity),*));


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
    struct Player {
        id: u32,
    }

    #[derive(Debug)]
    struct Enemy {
        hp: u32,
    }

    pub trait TestTrait {
        fn do_something(&self);
    }

    impl TestTrait for Player {
        fn do_something(&self) {
            println!("I'm a player {}", self.id)
        }
    }

    impl TestTrait for Enemy {
        fn do_something(&self) {
            println!("I'm an enemy {}", self.hp)
        }
    }

    pub trait SecondTestTrait {
        fn do_something_else(&mut self);
    }
    impl SecondTestTrait for Player {
        fn do_something_else(&mut self) {
            println!("player is the only one who implemented second trait")
        }
    }

    // name of world struct, contained entity types, contained entity traits.
    // entity types should be 'static
    world!(MyWorld, Player, Enemy; TestTrait, SecondTestTrait);

    #[test]
    fn test_visit_traits() {
        let mut world = MyWorld::default();

        world.player.insert(Player { id: 1 });
        world.enemy.insert(Enemy { hp: 10 });
        world.visit_test_trait(|e| e.do_something());
        world.visit_mut_second_test_trait(|e| e.do_something_else());
    }
}
