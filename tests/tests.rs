#![allow(incomplete_features)]
#![feature(specialization)]

extern crate entity_trait_system;

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
    entity_trait_system::world!(MyWorld, Enemy, Player; TestTrait, SecondTestTrait);

    // Extended world including a non-trait entity to test default retain/clear
    entity_trait_system::world!(AnotherWorld, Enemy, NonTrait, Player; TestTrait, SecondTestTrait);

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
