extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro]
pub fn make_visitor_methods(input: TokenStream) -> TokenStream {
    let mut source = input.into_iter();

    let base_name = source.next().unwrap();

    format!(
        "fn visit_{}(&mut self, c: &{}) -> Self::Result;",
        base_name.to_string().to_lowercase(),
        base_name.to_string()
    )
    .parse()
    .unwrap()
}

#[proc_macro]
pub fn impl_visitor_methods(input: TokenStream) -> TokenStream {
    let mut source = input.into_iter();

    let base_name = source.next().unwrap();

    format!(
        "visitor.visit_{}(self)",
        base_name.to_string().to_lowercase(),
    )
    .parse()
    .unwrap()
}

#[proc_macro]
pub fn impl_new_methods(input: TokenStream) -> TokenStream {
    let mut source = input.into_iter();

    let variant_name = source.next().unwrap();
    let fields = source
        .clone()
        .step_by(2)
        .zip(source.clone().skip(1).step_by(2))
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect::<Vec<_>>();

    format!(
        "pub fn new_{}({}) -> Self {{
            Self::{} {{
                {}
            }}
        }}",
        variant_name.to_string().to_lowercase(),
        fields
            .iter()
            .map(|(a, b)| format!("{}: {}", a.to_lowercase(), b))
            .collect::<Vec<_>>()
            .join(", "),
        variant_name.to_string(),
        fields
            .iter()
            .map(|(a, _)| a.to_lowercase())
            .collect::<Vec<_>>()
            .join(", "),
    )
    .parse()
    .unwrap()
}
