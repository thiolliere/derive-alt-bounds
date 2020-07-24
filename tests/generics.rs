use derive_alt_bounds::*;

fn assert_gen<T: Clone + core::fmt::Debug + PartialEq + Eq>() {}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
struct A<T>(core::marker::PhantomData<T>);

struct Nothing;

#[test]
fn test_a() {
	assert_gen::<A<Nothing>>();
}

// TODO TODO: test with recursive
