use derive_alt_bounds::*;

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
enum A {}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
enum B {
	B
}

#[test]
fn test_b() {
	assert!(B::B == B::B);
	let _ = B::B.clone();
}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
enum C {
	C1,
	C2,
}

#[test]
fn test_c() {
	assert!(C::C1 != C::C2);
	assert!(C::C1 == C::C1);
	assert!(C::C2 == C::C2);
}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
enum D {
	D1(u32, u64),
	D2 { d3: u32, d4: u8 },
}

#[test]
fn test_d() {
	assert!(D::D1(3, 4) != D::D1(3, 3));
	assert!(D::D1(3, 4) != D::D1(4, 4));
	assert!(D::D1(3, 4) == D::D1(3, 4));
	assert!(D::D2 { d3: 0, d4: 1 } == D::D2 { d3: 0, d4: 1 });
	assert!(D::D2 { d3: 0, d4: 1 } != D::D2 { d3: 1, d4: 1 });
	assert!(D::D2 { d3: 0, d4: 1 } != D::D2 { d3: 0, d4: 0 });
}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
struct E {}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
struct F(u32, u128);

#[test]
fn test_f() {
	assert!(F(0, 0) == F(0, 0));
	assert!(F(1, 0) != F(0, 0));
	assert!(F(0, 0) != F(0, 1));
}

#[derive(CloneTypBnds, DebugTypBnds, PartialEqTypBnds, EqTypBnds)]
struct G {
	g1: u32,
	g2: u128,
}

#[test]
fn test_g() {
	assert!(G { g1: 0, g2: 0 } == G { g1: 0, g2: 0 });
	assert!(G { g1: 1, g2: 0 } != G { g1: 0, g2: 0 });
	assert!(G { g1: 0, g2: 1 } != G { g1: 0, g2: 0 });
}
