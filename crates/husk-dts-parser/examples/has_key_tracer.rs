//! Tracer-bullet prototype for the `HasKey` pattern described in the
//! DTS importer v2 plan. This example intentionally generates many
//! zero-sized marker types and a single struct implementing several
//! of them to validate compile-time characteristics before wiring a
//! generator around the pattern.

use std::marker::PhantomData;

/// Global trait that associates a key marker with the value type it selects.
pub trait HasKey<K> {
    type Output;
}

macro_rules! define_keys {
    ($($name:ident),* $(,)?) => {
        $(
            #[allow(dead_code)]
            #[derive(Debug, Clone, Copy, Default)]
            pub struct $name;
        )*
    };
}

// 50 globally interned markers (Key0..Key49). In real generation this list is
// derived from every identifier observed in a `keyof` position across the
// imported graph.
define_keys!(
    Key0, Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9, Key10, Key11, Key12, Key13, Key14,
    Key15, Key16, Key17, Key18, Key19, Key20, Key21, Key22, Key23, Key24, Key25, Key26, Key27,
    Key28, Key29, Key30, Key31, Key32, Key33, Key34, Key35, Key36, Key37, Key38, Key39, Key40,
    Key41, Key42, Key43, Key44, Key45, Key46, Key47, Key48, Key49,
);

/// A sample struct to exercise `HasKey` impls.
pub struct User {
    pub id: u64,
    pub name: String,
    pub email: String,
    pub login_count: u32,
}

impl HasKey<Key0> for User {
    type Output = u64;
}
impl HasKey<Key1> for User {
    type Output = String;
}
impl HasKey<Key2> for User {
    type Output = String;
}
impl HasKey<Key3> for User {
    type Output = u32;
}

/// Generic accessor that enforces the `HasKey` contract at compile time while
/// staying zero-cost.
pub fn get<T, K>(_obj: &T, _key: K) -> PhantomData<<T as HasKey<K>>::Output>
where
    T: HasKey<K>,
{
    PhantomData
}

fn main() {
    let user = User {
        id: 42,
        name: "ada".to_string(),
        email: "ada@example.com".to_string(),
        login_count: 7,
    };

    // These lines ensure monomorphization occurs for several markers.
    let _id = get(&user, Key0);
    let _name = get(&user, Key1);
    let _email = get(&user, Key2);
    let _count = get(&user, Key3);

    // Keep the example executable but side-effect free.
    let _ = (_id, _name, _email, _count);
}
