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
