#![deny(rust_2018_idioms)]

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    punctuated::Punctuated, spanned::Spanned, Attribute, Data, DeriveInput, Error, Fields,
    FieldsNamed, Generics, Ident, LitInt, Result, Token, WhereClause,
};

#[proc_macro_derive(DeserializeFromBytes, attributes(trailing_padding, padding))]
pub fn derive_deserialize_from_bytes(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    fn inner(input: TokenStream) -> Result<TokenStream> {
        // Parse derive
        let (name, data, generics, trailing_padding) = parse_derive(input)?;

        // Generics
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let mut where_clause = where_clause.cloned().unwrap_or_else(|| WhereClause {
            where_token: <Token![where]>::default(),
            predicates: Punctuated::new(),
        });

        // Deserialize fields
        let fields = match data {
            Data::Struct(data) => match data.fields {
                Fields::Named(fields) => fields,
                Fields::Unnamed(_) => {
                    return Err(Error::new(Span::call_site(), "only named structs are supported"))
                }
                Fields::Unit => {
                    return Err(Error::new(Span::call_site(), "only named structs are supported"))
                }
            },
            Data::Enum(_) => {
                return Err(Error::new(Span::call_site(), "only named structs are supported"))
            }
            Data::Union(_) => {
                return Err(Error::new(Span::call_site(), "only named structs are supported"))
            }
        };
        let deserialized_fields_ne =
            deserialize_fields(&fields, &Ident::new("deserialize_ne", Span::call_site()))?;
        let deserialized_fields_le =
            deserialize_fields(&fields, &Ident::new("deserialize_le", Span::call_site()))?;
        let deserialized_fields_be =
            deserialize_fields(&fields, &Ident::new("deserialize_be", Span::call_site()))?;

        // Return value
        let ret = {
            let fields = fields
                .named
                .iter()
                .map(|field| {
                    let ident = field.ident.as_ref().unwrap();
                    let deserialized_name =
                        Ident::new(&format!("deserialized_{}", ident), ident.span());
                    quote! { #ident: #deserialized_name }
                })
                .collect::<Vec<_>>();
            quote! {
                Ok(Self {
                    #(#fields),*
                })
            }
        };

        // Add field types to where clause
        for field in &fields.named {
            let ty = &field.ty;
            where_clause.predicates.push(syn::parse_quote! {
                #ty: ::bserde::DeserializeFromBytes
            });
        }

        // Deserialize trailing padding
        let deserialize_trailing_padding = match trailing_padding {
            0 => None,
            n => Some(quote! { read.read_exact(&mut [0; #n])?; }),
        };

        Ok(quote! {
            impl #impl_generics ::bserde::DeserializeFromBytes for #name #ty_generics #where_clause {
                fn deserialize_ne<R: ::std::io::Read>(mut read: R) -> ::std::io::Result<Self> {
                    // Deserialize fields
                    #deserialized_fields_ne

                    // Deserialize trailing padding
                    #deserialize_trailing_padding

                    #ret
                }

                fn deserialize_le<R: ::std::io::Read>(mut read: R) -> ::std::io::Result<Self> {
                    // Deserialize fields
                    #deserialized_fields_le

                    // Deserialize trailing padding
                    #deserialize_trailing_padding

                    #ret
                }

                fn deserialize_be<R: ::std::io::Read>(mut read: R) -> ::std::io::Result<Self> {
                    // Deserialize fields
                    #deserialized_fields_be

                    // Deserialize trailing padding
                    #deserialize_trailing_padding

                    #ret
                }
            }
        })
    }

    match inner(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro_derive(SerializeAsBytes, attributes(trailing_padding, padding))]
pub fn derive_serialize_as_bytes(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    fn inner(input: TokenStream) -> Result<TokenStream> {
        // Parse derive
        let (name, data, generics, trailing_padding) = parse_derive(input)?;

        // Generics
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let mut where_clause = where_clause.cloned().unwrap_or_else(|| WhereClause {
            where_token: <Token![where]>::default(),
            predicates: Punctuated::new(),
        });

        // Serialize fields
        let fields = match data {
            Data::Struct(data) => match data.fields {
                Fields::Named(fields) => fields,
                Fields::Unnamed(_) => {
                    return Err(Error::new(Span::call_site(), "only named structs are supported"))
                }
                Fields::Unit => {
                    return Err(Error::new(Span::call_site(), "only named structs are supported"))
                }
            },
            Data::Enum(_) => {
                return Err(Error::new(Span::call_site(), "only named structs are supported"))
            }
            Data::Union(_) => {
                return Err(Error::new(Span::call_site(), "only named structs are supported"))
            }
        };
        let serialized_fields_ne =
            serialize_fields(&fields, &Ident::new("serialize_ne", Span::call_site()))?;
        let serialized_fields_le =
            serialize_fields(&fields, &Ident::new("serialize_le", Span::call_site()))?;
        let serialized_fields_be =
            serialize_fields(&fields, &Ident::new("serialize_be", Span::call_site()))?;

        // Add field types to where clause
        for field in &fields.named {
            let ty = &field.ty;
            where_clause.predicates.push(syn::parse_quote! {
                #ty: ::bserde::SerializeAsBytes
            });
        }

        // Serialize trailing padding
        let serialize_trailing_padding = match trailing_padding {
            0 => None,
            n => Some(quote! { write.write_all(&[0; #n])?; }),
        };

        Ok(quote! {
            impl #impl_generics ::bserde::SerializeAsBytes for #name #ty_generics #where_clause {
                fn serialize_ne<W: ::std::io::Write>(&self, mut write: W) -> ::std::io::Result<()> {
                    // Serialize fields
                    #serialized_fields_ne

                    // Serialize trailing padding
                    #serialize_trailing_padding

                    Ok(())
                }

                fn serialize_le<W: ::std::io::Write>(&self, mut write: W) -> ::std::io::Result<()> {
                    // Serialize fields
                    #serialized_fields_le

                    // Serialize trailing padding
                    #serialize_trailing_padding

                    Ok(())
                }

                fn serialize_be<W: ::std::io::Write>(&self, mut write: W) -> ::std::io::Result<()> {
                    // Serialize fields
                    #serialized_fields_be

                    // Serialize trailing padding
                    #serialize_trailing_padding

                    Ok(())
                }
            }
        })
    }

    match inner(input.into()) {
        Ok(tokens) => tokens.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn parse_derive(input: TokenStream) -> Result<(Ident, Data, Generics, usize)> {
    let ast = syn::parse2::<DeriveInput>(input).unwrap();
    let trailing_padding = calc_padding(&ast.attrs, "trailing_padding")?;

    Ok((ast.ident, ast.data, ast.generics, trailing_padding))
}

fn deserialize_fields(fields: &FieldsNamed, method: &Ident) -> Result<TokenStream> {
    fields
        .named
        .iter()
        .map(|field| {
            let deserialize_padding = match calc_padding(&field.attrs, "padding")? {
                0 => None,
                n => Some(quote! { read.read_exact(&mut [0; #n])?; }),
            };
            let ty = &field.ty;
            let ident = field.ident.as_ref().unwrap();
            let deserialized_name = Ident::new(&format!("deserialized_{}", ident), ident.span());

            Ok(quote! {
                #deserialize_padding
                #[allow(non_snake_case)]
                let #deserialized_name = <#ty as ::bserde::DeserializeFromBytes>::#method(&mut read)?;
            })
        })
        .collect::<Result<TokenStream>>()
}

fn serialize_fields(fields: &FieldsNamed, method: &Ident) -> Result<TokenStream> {
    fields
        .named
        .iter()
        .map(|field| {
            let serialize_padding = match calc_padding(&field.attrs, "padding")? {
                0 => None,
                n => Some(quote! { write.write_all(&[0; #n])?; }),
            };
            let ident = &field.ident;

            Ok(quote! {
                #serialize_padding
                ::bserde::SerializeAsBytes::#method(&self.#ident, &mut write)?;
            })
        })
        .collect::<Result<TokenStream>>()
}

fn calc_padding(attrs: &[Attribute], padding_name: &str) -> Result<usize> {
    let mut padding = 0usize;
    for attr in attrs {
        if attr.path().is_ident(padding_name) {
            let lit = attr.parse_args::<LitInt>()?;
            let n = lit.base10_parse::<usize>()?;

            padding = padding.checked_add(n).ok_or_else(|| {
                Error::new(attr.span(), format!("overflowing {padding_name} attribute"))
            })?;
        }
    }
    Ok(padding)
}
