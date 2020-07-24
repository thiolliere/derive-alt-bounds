allow to derive Clone, Eq, Debug, PartialEq with better bounds than build-in one, e.g:
```rust
trait A {
	type B;
}
#[derive(CloneTypBnds)]
// Will derive Clone with where_clause: `where T::B: Clone`
// Instead of build-in derive Clone which bounds `T: Clone`
struct C<T: A>(T::B);
```

Though bounding on recursive types is still not perfect, e.g for the type:
```rust
#[derive(CloneTypBnds)]
// It can't bound `where Vec<A<T>>: Clone` because compiler return error
// `overflow evaluating the requirement...`
struct A<T>(Vec<A<T>>, PhantomData<T>);
```

Bounds for recursive type are still kind of WIP.
