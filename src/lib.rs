//! TODO TODO: many traits, some difficulties with recursive types.

use proc_macro::TokenStream;

#[proc_macro_derive(CloneTypBnds)]
pub fn derive_clone_bound_type(input: TokenStream) -> TokenStream {
	use syn::spanned::Spanned;

	let mut input: syn::DeriveInput = match syn::parse(input) {
		Ok(input) => input,
		Err(e) => return e.to_compile_error().into(),
	};

	if let Err(e) = trait_bounds::add(
		&input.ident,
		&mut input.generics,
		&input.data,
		syn::parse_quote!(core::clone::Clone),
		false,
	) {
		return e.to_compile_error().into();
	}

	let name = &input.ident;
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	let impl_ = match input.data {
		syn::Data::Struct(struct_) => match struct_.fields {
			syn::Fields::Named(named) => {
				let fields = named.named.iter()
					.map(|i| i.ident.as_ref().expect("named fields have ident"))
					.map(|i| quote::quote!( #i: self.#i.clone() ));

				quote::quote!( Self { #( #fields, )* } )
			},
			syn::Fields::Unnamed(unnamed) => {
				let fields = unnamed.unnamed.iter().enumerate()
					.map(|(i, _)| syn::Index::from(i))
					.map(|i| quote::quote!( self.#i.clone() ));

				quote::quote!( Self ( #( #fields, )* ) )
			},
			syn::Fields::Unit => {
				quote::quote!( Self )
			}
		},
		syn::Data::Enum(enum_) => {
			let variants = enum_.variants.iter()
				.map(|variant| {
					let ident = &variant.ident;
					match &variant.fields {
						syn::Fields::Named(named) => {
							let captured = named.named.iter()
								.map(|i| i.ident.as_ref().expect("named fields have ident"));
							let cloned = captured.clone()
								.map(|i| quote::quote!( #i: #i.clone() ));
							quote::quote!(
								Self::#ident { #( ref #captured, )* } => Self::#ident { #( #cloned, )*}
							)
						},
						syn::Fields::Unnamed(unnamed) => {
							let captured = unnamed.unnamed.iter().enumerate()
								.map(|(i, f)| syn::Ident::new(&format!("_{}", i), f.span()));
							let cloned = captured.clone().map(|i| quote::quote!( #i.clone() ));
							quote::quote!(
								Self::#ident ( #( ref #captured, )* ) => Self::#ident ( #( #cloned, )*)
							)
						},
						syn::Fields::Unit => quote::quote!( Self::#ident => Self::#ident ),
					}
				});

			quote::quote!( match *self {
				#( #variants, )*
			})
		},
		syn::Data::Union(_) => {
			let msg ="Union type not supported by `derive(CloneTypBnds)`";
			return syn::Error::new(input.span(), msg).to_compile_error().into()
		},
	};

	quote::quote!(
		const _: () = {
			impl #impl_generics core::clone::Clone for #name #ty_generics #where_clause {
				fn clone(&self) -> Self {
					#impl_
				}
			}
		};
	).into()
}

#[proc_macro_derive(DebugTypBnds)]
pub fn derive_debug_bound_type(input: TokenStream) -> TokenStream {
	use syn::spanned::Spanned;

	let mut input: syn::DeriveInput = match syn::parse(input) {
		Ok(input) => input,
		Err(e) => return e.to_compile_error().into(),
	};

	if let Err(e) = trait_bounds::add(
		&input.ident,
		&mut input.generics,
		&input.data,
		syn::parse_quote!(core::fmt::Debug),
		false,
	) {
		return e.to_compile_error().into();
	}

	let input_ident = &input.ident;
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	let impl_ = match input.data {
		syn::Data::Struct(struct_) => match struct_.fields {
			syn::Fields::Named(named) => {
				let fields = named.named.iter()
					.map(|i| i.ident.as_ref().expect("named fields have ident"))
					.map(|i| quote::quote!( .field(stringify!(#i), &self.#i) ));

				quote::quote!( fmt.debug_struct(stringify!(#input_ident))
					#( #fields )*
					.finish()
				)
			},
			syn::Fields::Unnamed(unnamed) => {
				let fields = unnamed.unnamed.iter().enumerate()
					.map(|(i, _)| syn::Index::from(i))
					.map(|i| quote::quote!( .field(&self.#i) ));

				quote::quote!( fmt.debug_tuple(stringify!(#input_ident))
					#( #fields )*
					.finish()
				)
			},
			syn::Fields::Unit => quote::quote!( fmt.write_str(stringify!(#input_ident)) ),
		},
		syn::Data::Enum(enum_) => {
			let variants = enum_.variants.iter()
				.map(|variant| {
					let ident = &variant.ident;
					let full_variant_str = format!("{}::{}", input_ident, ident);
					match &variant.fields {
						syn::Fields::Named(named) => {
							let captured = named.named.iter()
								.map(|i| i.ident.as_ref().expect("named fields have ident"));
							let debuged = captured.clone()
								.map(|i| quote::quote!( .field(stringify!(#i), &#i) ));
							quote::quote!(
								Self::#ident { #( ref #captured, )* } => {
									fmt.debug_struct(#full_variant_str)
										#( #debuged )*
										.finish()
								}
							)
						},
						syn::Fields::Unnamed(unnamed) => {
							let captured = unnamed.unnamed.iter().enumerate()
								.map(|(i, f)| syn::Ident::new(&format!("_{}", i), f.span()));
							let debuged = captured.clone().map(|i| quote::quote!( .field(&#i) ));
							quote::quote!(
								Self::#ident ( #( ref #captured, )* ) => {
									fmt.debug_tuple(#full_variant_str)
										#( #debuged )*
										.finish()
								}
							)
						},
						syn::Fields::Unit => quote::quote!(
							Self::#ident => fmt.write_str(#full_variant_str)
						),
					}
				});

			quote::quote!( match *self {
				#( #variants, )*
			})
		},
		syn::Data::Union(_) => {
			let msg ="Union type not supported by `derive(DebugTypBnds)`";
			return syn::Error::new(input.span(), msg).to_compile_error().into()
		},
	};

	quote::quote!(
		const _: () = {
			impl #impl_generics core::fmt::Debug for #input_ident #ty_generics #where_clause {
				fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {
					#impl_
				}
			}
		};
	).into()
}


#[proc_macro_derive(PartialEqTypBnds)]
pub fn derive_partial_eq_bound_type(input: TokenStream) -> TokenStream {
	use syn::spanned::Spanned;

	let mut input: syn::DeriveInput = match syn::parse(input) {
		Ok(input) => input,
		Err(e) => return e.to_compile_error().into(),
	};

	if let Err(e) = trait_bounds::add(
		&input.ident,
		&mut input.generics,
		&input.data,
		syn::parse_quote!(core::cmp::PartialEq),
		false,
	) {
		return e.to_compile_error().into();
	}

	let name = &input.ident;
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	let impl_ = match input.data {
		syn::Data::Struct(struct_) => match struct_.fields {
			syn::Fields::Named(named) => {
				let fields = named.named.iter()
					.map(|i| i.ident.as_ref().expect("named fields have ident"))
					.map(|i| quote::quote!( self.#i == other.#i ));

				quote::quote!( true #( && #fields )* )
			},
			syn::Fields::Unnamed(unnamed) => {
				let fields = unnamed.unnamed.iter().enumerate()
					.map(|(i, _)| syn::Index::from(i))
					.map(|i| quote::quote!( self.#i == other.#i ));

				quote::quote!( true #( && #fields )* )
			},
			syn::Fields::Unit => {
				quote::quote!( true )
			}
		},
		syn::Data::Enum(enum_) => {
			let variants = enum_.variants.iter()
				.map(|variant| {
					let ident = &variant.ident;
					match &variant.fields {
						syn::Fields::Named(named) => {
							let names = named.named.iter()
								.map(|i| i.ident.as_ref().expect("named fields have ident"));
							let names_bis = names.clone()
								.map(|i| {
									syn::Ident::new(
										&format!("{}_bis", i),
										proc_macro2::Span::call_site(),
									)
								});

							let capture = names.clone();
							let capture_bis = names.clone().zip(names_bis.clone())
								.map(|(i, i_bis)| quote::quote!(#i: #i_bis));
							let eq = names.zip(names_bis)
								.map(|(i, i_bis)| quote::quote!(#i == #i_bis));
							quote::quote!(
								(
									Self::#ident { #( #capture, )* },
									Self::#ident { #( #capture_bis, )* },
								) => true #( && #eq )*
							)
						},
						syn::Fields::Unnamed(unnamed) => {
							let names = unnamed.unnamed.iter().enumerate()
								.map(|(i, f)| syn::Ident::new(&format!("_{}", i), f.span()));
							let names_bis = unnamed.unnamed.iter().enumerate()
								.map(|(i, f)| syn::Ident::new(&format!("_{}_bis", i), f.span()));
							let eq = names.clone().zip(names_bis.clone())
								.map(|(i, i_bis)| quote::quote!(#i == #i_bis));
							quote::quote!(
								(
									Self::#ident ( #( #names, )* ),
									Self::#ident ( #( #names_bis, )* ),
								) => true #( && #eq )*
							)
						},
						syn::Fields::Unit => quote::quote!( (Self::#ident, Self::#ident) => true ),
					}
				});

			quote::quote!( match (&self, other) {
				#( #variants, )*
				_ => false,
			})
		},
		syn::Data::Union(_) => {
			let msg ="Union type not supported by `derive(CloneTypBnds)`";
			return syn::Error::new(input.span(), msg).to_compile_error().into()
		},
	};

	quote::quote!(
		const _: () = {
			impl #impl_generics core::cmp::PartialEq for #name #ty_generics #where_clause {
				fn eq(&self, other: &Self) -> bool {
					#impl_
				}
			}
		};
	).into()
}

#[proc_macro_derive(EqTypBnds)]
pub fn derive_eq_bound_type(input: TokenStream) -> TokenStream {
	let mut input: syn::DeriveInput = match syn::parse(input) {
		Ok(input) => input,
		Err(e) => return e.to_compile_error().into(),
	};

	if let Err(e) = trait_bounds::add(
		&input.ident,
		&mut input.generics,
		&input.data,
		syn::parse_quote!(core::cmp::Eq),
		false,
	) {
		return e.to_compile_error().into();
	}

	let name = &input.ident;
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

	quote::quote!(
		const _: () = {
			impl #impl_generics core::cmp::Eq for #name #ty_generics #where_clause {}
		};
	).into()
}

mod trait_bounds {
	use std::iter;

	use proc_macro2::Ident;
	use syn::{
		spanned::Spanned,
		visit::{self, Visit},
		Generics, Result, Type, TypePath,
	};

	/// Visits the ast and checks if one of the given idents is found.
	struct ContainIdents<'a> {
		result: bool,
		idents: &'a[Ident]
	}

	impl<'a, 'ast> Visit<'ast> for ContainIdents<'a> {
		fn visit_ident(&mut self, i: &'ast Ident) {
			if self.idents.iter().any(|id| id == i) {
				self.result = true;
			}
		}
	}

	/// Checks if the given type contains one of the given idents.
	fn type_contain_idents(ty: &Type, idents: &[Ident]) -> bool {
		let mut visitor = ContainIdents { result: false, idents };
		visitor.visit_type(ty);
		visitor.result
	}

	/// Visits the ast and checks if the a type path starts with the given ident.
	struct TypePathStartsWithIdent<'a> {
		result: bool,
		ident: &'a Ident
	}

	impl<'a, 'ast> Visit<'ast> for TypePathStartsWithIdent<'a> {
		fn visit_type_path(&mut self, i: &'ast TypePath) {
			if let Some(segment) = i.path.segments.first() {
				if &segment.ident == self.ident {
					self.result = true;
					return;
				}
			}

			visit::visit_type_path(self, i);
		}
	}

	/// Checks if the given type path or any containing type path starts with the given ident.
	fn type_path_or_sub_starts_with_ident(ty: &TypePath, ident: &Ident) -> bool {
		let mut visitor = TypePathStartsWithIdent { result: false, ident };
		visitor.visit_type_path(ty);
		visitor.result
	}

	/// Checks if the given type or any containing type path starts with the given ident.
	fn type_or_sub_type_path_starts_with_ident(ty: &Type, ident: &Ident) -> bool {
		let mut visitor = TypePathStartsWithIdent { result: false, ident };
		visitor.visit_type(ty);
		visitor.result
	}

	/// Visits the ast and collects all type paths that do not start or contain the given ident.
	///
	/// Returns `T`, `N`, `A` for `Vec<(Recursive<T, N>, A)>` with `Recursive` as ident.
	struct FindTypePathsNotStartOrContainIdent<'a> {
		result: Vec<TypePath>,
		ident: &'a Ident
	}

	impl<'a, 'ast> Visit<'ast> for FindTypePathsNotStartOrContainIdent<'a> {
		fn visit_type_path(&mut self, i: &'ast TypePath) {
			if type_path_or_sub_starts_with_ident(i, &self.ident) {
				visit::visit_type_path(self, i);
			} else {
				self.result.push(i.clone());
			}
		}
	}

	/// Collects all type paths that do not start or contain the given ident in the given type.
	///
	/// Returns `T`, `N`, `A` for `Vec<(Recursive<T, N>, A)>` with `Recursive` as ident.
	fn find_type_paths_not_start_or_contain_ident(ty: &Type, ident: &Ident) -> Vec<TypePath> {
		let mut visitor = FindTypePathsNotStartOrContainIdent { result: Vec::new(), ident };
		visitor.visit_type(ty);
		visitor.result
	}

	/// Add required trait bounds to all generic types.
	pub fn add(
		input_ident: &Ident,
		generics: &mut Generics,
		data: &syn::Data,
		codec_bound: syn::Path,
		dumb_trait_bounds: bool,
	) -> Result<()> {
		let ty_params = generics.type_params().map(|p| p.ident.clone()).collect::<Vec<_>>();
		if ty_params.is_empty() {
			return Ok(());
		}

		let codec_types = get_types_to_add_trait_bound(input_ident, data, &ty_params, dumb_trait_bounds)?;


		if !codec_types.is_empty() {
			let where_clause = generics.make_where_clause();

			codec_types
				.into_iter()
				.for_each(|ty| {
					where_clause.predicates.push(syn::parse_quote!(#ty : #codec_bound))
				});
		}

		Ok(())
	}

	/// Returns all types that must be added to the where clause with the respective trait bound.
	fn get_types_to_add_trait_bound(
		input_ident: &Ident,
		data: &syn::Data,
		ty_params: &[Ident],
		dumb_trait_bound: bool,
	) -> Result<Vec<Type>> {
		if dumb_trait_bound {
			Ok(ty_params.iter().map(|t| syn::parse_quote!( #t )).collect())
		} else {
			let res = collect_types(&data)?
				.into_iter()
				// Only add a bound if the type uses a generic
				.filter(|ty| type_contain_idents(ty, &ty_params))
				// If a struct is cotaining itself as field type, we can not add this type into the where clause.
				// This is required to work a round the following compiler bug: https://github.com/rust-lang/rust/issues/47032
				.flat_map(|ty| {
					find_type_paths_not_start_or_contain_ident(&ty, input_ident)
						.into_iter()
						.map(|ty| Type::Path(ty.clone()))
						// Remove again types that do not contain any of our generic parameters
						.filter(|ty| type_contain_idents(ty, &ty_params))
						// Add back the original type, as we don't want to loose him.
						.chain(iter::once(ty))
				})
				// Remove all remaining types that start/contain the input ident to not have them in the where clause.
				.filter(|ty| !type_or_sub_type_path_starts_with_ident(ty, input_ident))
				.collect();

			Ok(res)
		}
	}

	fn collect_types(
		data: &syn::Data,
	) -> Result<Vec<syn::Type>> {
		use syn::*;

		let types = match *data {
			Data::Struct(ref data) => match &data.fields {
				| Fields::Named(FieldsNamed { named: fields , .. })
				| Fields::Unnamed(FieldsUnnamed { unnamed: fields, .. }) => {
					fields.iter()
						.map(|f| f.ty.clone())
						.collect()
				},

				Fields::Unit => { Vec::new() },
			},

			Data::Enum(ref data) => data.variants.iter()
				.flat_map(|variant| {
					match &variant.fields {
						| Fields::Named(FieldsNamed { named: fields , .. })
						| Fields::Unnamed(FieldsUnnamed { unnamed: fields, .. }) => {
							fields.iter()
								.map(|f| f.ty.clone())
								.collect()
						},

						Fields::Unit => { Vec::new() },
					}
				}).collect(),

			Data::Union(ref data) => return Err(Error::new(
				data.union_token.span(),
				"Union types are not supported."
			)),
		};

		Ok(types)
	}
}
