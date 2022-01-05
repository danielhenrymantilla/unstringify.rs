//! # `unstringify!`
//!
//! See [the documentation of the macro for more info][`unstringify!`]

/// For compat with older versions of `rustc`.
extern crate proc_macro;

use ::proc_macro::{*,
    TokenTree as TT,
};

#[macro_use]
mod utils;

struct Input {
    tokenized: TokenStream,
    metavar: Ident,
    template: TokenStream,
}

/// Reverse of [`stringify!`]: tokenize an input string literal.
///
/// ## Basic example
///
/// ```rust
/// use ::unstringify::unstringify;
///
/// unstringify!(r#"
///     fn main ()
///     {
///         println!("Hello, World!");
///     }
/// "#);
/// ```
///
/// Or, equivalently:
///
/// ```rust
/// use ::unstringify::unstringify;
///
/// unstringify!(stringify! {
///     fn main ()
///     {
///         println!("Hello, World!");
///     }
/// });
/// ```
///
/// <details><summary>▶ A more interesting example</summary>
///
/// A (non-procedural!) macro to evaluate the rust code snippets inside
/// docstrings:
///
/// ```rust,should_panic
/// use ::unstringify::unstringify;
///
/// macro_rules! eval_docstrings {(
///     $(
///         #[doc = $doc:tt]
///     )*
/// ) => (
///     extract_code_snippets! {
///         @find_start
///         $($doc)*
///     }
/// )}
///
/// macro_rules! extract_code_snippets {
///     (
///         @find_start
///         r" ```rust"
///         $($rest:tt)*
///     ) => (
///         extract_code_snippets! {
///             @accumulate_into []
///             $($rest)*
///         }
///     );
///
///     (
///         @find_start
///         $otherwise_ignored:tt // ≠ " ```rust"
///         $($rest:tt)*
///     ) => (
///         extract_code_snippets! {
///             @find_start
///             $($rest)*
///         }
///     );
///
///     (
///         @find_start
///         // No lines left
///     ) => (
///         // The end.
///     );
///
///     // End of code snippet found,
///     // TIME TO EVALUATE THE CODE!
///     (
///         @accumulate_into [ $($lines:tt)* ]
///         r" ```"
///         $($rest:tt)*
///     ) => (
///         // evaluate the code...
///         unstringify!(concat!(
///             $($lines),*
///         ));
///         // ... and rince and repeat with the remaining docstrings
///         extract_code_snippets! {
///             @find_start
///             $($rest)*
///         }
///     );
///
///     // Basic recursion step: accumulate a non-terminating line
///     (
///         @accumulate_into [ $($lines:tt)* ]
///         $current_line:tt // ≠ " ```"
///         $($rest:tt)*
///     ) => (
///         extract_code_snippets! {
///             @accumulate_into [ $($lines)* $current_line ]
///             $($rest)*
///         }
///     );
/// }
///
/// eval_docstrings! {
///     /// This is a comment.
///     /// As ordinary as they make them.
///     ///
///     /// And yet...
///     /// Sometimes...
///     ///
///     /// > A code snippet appears!
///     ///
///     /// ```rust
///     /// panic!("Successfully managed to evaluate this panic (and thus panic)");
///     /// ```
///     ///
///     /// Impressive, ain't it?
/// }
/// ```
///
/// ___
///
/// </details>
///
/// ## Remarks
///
/// This intuitive API very quickly encounters limitations, related not the
/// macro itself, but rather to the way Rust expands macros.
///
/// So, for instance, the following assertion fails:
///
/// ```rust,should_panic
/// # use ::unstringify::unstringify;
/// #
/// assert_eq!(
///     stringify!(unstringify!("example")),
///     "example",
/// );
/// ```
///
/// Indeed, in the above code the macro `stringify!` is called _before_
/// `unstringify!`, so what happens is `stringify!` simply stringifies its input
/// tokens, _verbatim_, without evaluating them: `'unstringify!("example")'`. 🤦
///
/// To solve that, [`unstringify!`] features "preprocessor" capabilities
/// similar to [`::paste::paste!`](https://docs.rs/paste), that allow to
/// circumvent this limitation, by doing:
///
/// ```rust
/// # use ::unstringify::unstringify;
/// #
/// assert_eq!(
///     unstringify!(let $tokens = unstringify!("example") in {
///         stringify!($tokens)
///     }),
///     "example",
/// );
/// ```
///
/// ___
///
/// Also, for the same reason but reversed this time, the input fed to
/// [`unstringify!`] cannot be eagerly macro-expanded.
///
/// This means that the following fails:
///
/// ```rust,compile_fail
/// # use ::unstringify::unstringify;
/// #
/// macro_rules! my_macro {() => ("fn main () {}")}
///
/// unstringify!(my_macro!());
/// ```
///
///   - The workaround is to define things such as `my_macro!` using, for
///     instance, the callback pattern:
///
///     ```rust
///     # use ::unstringify::unstringify;
///     #
///     macro_rules! my_macro {(
///         => $callback:ident !
///     ) => (
///         $callback! { "fn main () {}" }
///     )}
///
///     my_macro!(=> unstringify!);
///     ```
///
/// That being said, the astute reader may retort:
///
/// > But wait, doesn't your second example within this documentation showcase
/// > `unstringify!(stringify! { ...  })`?
///
/// And indeed it does. This is achieved by hard-coding the (basic) logic of
/// `stringify!` and `concat!` inside the [`unstringify!`] macro (for instance,
/// when [`unstringify!`] stumbles upon a `stringify! { ... }` (which is _not_, I
/// repeat, a _verbatim_ string literal), it decides to simply emit the inner
/// `...`).
#[proc_macro] pub
fn unstringify (input: TokenStream)
  -> TokenStream
{
    match tokenize_string_literal_or_concat_or_stringify(
        input.clone().into_iter().peekable()
    )
    {
        | Ok((tokenized, mut remaining)) => if remaining.next().is_none() {
            return tokenized;
        },
        | _ => {}
    }
    let Input {
        tokenized, metavar, template,
    } = match let_unstringify(input) {
        | Ok(it) => it,
        | Err((span, err_msg)) => {
            macro_rules! spanned {($expr:expr) => (
                match $expr { mut expr => {
                    expr.set_span(span);
                    expr
                }}
            )}
            return ts![
                Ident::new("compile_error", span),
                spanned!(Punct::new('!', Spacing::Alone)),
                spanned!(ts![ (
                    Literal::string(&*err_msg),
                )]),
                spanned!(Punct::new(';', Spacing::Alone)),
            ];
        },
    };
    map_replace(&metavar.to_string(), &tokenized, template)
}

/// `let $var = unstringify!("...") in { ... }`
fn let_unstringify (input: TokenStream)
  -> Result<
        Input,
        (Span, ::std::borrow::Cow<'static, str>),
    >
{
    let mut tokens = input.into_iter().peekable();
    unwrap_next_token! {
        if let TT::Ident(ident) = tokens.next(),
            if (ident.to_string() == "let")
        {} else {
            failwith!("expected `let`");
        }
    }
    unwrap_next_token! {
        if let TT::Punct(p) = tokens.next(),
            if (p.as_char() == '$')
        {} else {
            failwith!("expected `$`");
        }
    }
    let metavar = unwrap_next_token! {
        if let TT::Ident(it) = tokens.next(), { it } else {
            failwith!("expected an identifier");
        }
    };
    unwrap_next_token! {
        if let TT::Punct(p) = tokens.next(),
            if (p.as_char() == '=')
        {} else {
            failwith!("expected `=`");
        }
    }
    unwrap_next_token! {
        if let TT::Ident(ident) = tokens.next(),
            if (ident.to_string() == "unstringify")
        {} else {
            failwith!("expected `unstringify`");
        }
    }
    unwrap_next_token! {
        if let TT::Punct(p) = tokens.next(),
            if (p.as_char() == '!')
        {} else {
            failwith!("expected `!`");
        }
    }
    let tokenized: TokenStream = {
        let tokenize_args = unwrap_next_token! {
            if let TT::Group(group) = tokens.next(),
                if (matches!(group.delimiter(), Delimiter::Parenthesis))
            {
                group.stream().into_iter()
            } else {
                failwith!("expected `( ... )`");
            }
        };
        let (tokenized, mut remaining) =
            tokenize_string_literal_or_concat_or_stringify(
                tokenize_args.into_iter().peekable(),
            )?
        ;
        if let Some(extraneous_tt) = remaining.next() {
            return Err((
                extraneous_tt.span(),
                "extraneous token(s)".into(),
            ));
        }
        tokenized
    };
    unwrap_next_token! {
        if let TT::Ident(in_) = tokens.next(),
            if (in_.to_string() == "in")
        {} else {
            failwith!("expected `;`");
        }
    }
    let rest = unwrap_next_token! {
        if let TT::Group(group) = tokens.next(),
        {
            group.stream()
        } else {
            failwith!("expected `{ ... }` or `( ... )` or `[ ... ]`");
        }
    };
    if let Some(extraneous_tt) = tokens.next() {
        return Err((
            extraneous_tt.span(),
            "extraneous token(s)".into(),
        ));
    }
    Ok(Input {
        tokenized,
        metavar,
        template: rest,
    })
}

fn map_replace (
    metavar: &'_ String,
    tokenized: &'_ TokenStream,
    tokens: TokenStream
) -> TokenStream
{
    let mut tokens = tokens.into_iter().peekable();
    let mut ret = TokenStream::new();
    loop {
        match (tokens.next(), tokens.peek()) {
            | (
                Some(TT::Punct(dollar)),
                Some(TT::Ident(ident)),
            )
                if  dollar.as_char() == '$'
                &&  ident.to_string() == *metavar
            => {
                drop(tokens.next());
                ret.extend(tokenized.clone());
            },

            | (Some(TT::Group(group)), _) => {
                ret.extend(Some(TT::Group(Group::new(
                    group.delimiter(),
                    map_replace(metavar, tokenized, group.stream()),
                ))));
            },

            | (None, _) => break,

            | (tt, _) => ret.extend(tt),
        }
    }
    ret
}

type Tokens = ::core::iter::Peekable<token_stream::IntoIter>;

/// Input may be a:
///
///   - a string literal (terminal);
///
///   - a `stringify! { ... }` call (verbatim);
///
///   - a `concat!(...)` call whose args can be any of these three options:
///     recurse.
///
/// To recurse, especially when wanting to parse a comma-separated sequence of
/// expressions, we return not only the successfully parsed-and-then-tokenized
/// input, we also return the trailing tokens, to keep iterating the logic
/// if they start with a `,`.
fn tokenize_string_literal_or_concat_or_stringify (
    mut tokens: Tokens,
) -> Result<
        (TokenStream, Tokens),
        (Span, ::std::borrow::Cow<'static, str>),
    >
{Ok({
    let recurse = tokenize_string_literal_or_concat_or_stringify;
    macro_rules! err_msg {() => (
        "expected \
            a string literal, \
            a verbatim `stringify!` call, \
            or a verbatim `concat!` call.\
        "
    )}
    let mut s: String;
    let ret = match tokens.next() {
        // Un-group the `$var` metavariables.
        | Some(TT::Group(group))
            if matches!(group.delimiter(), Delimiter::None)
        => {
            let mut flattened = group.stream();
            flattened.extend(tokens);
            return recurse(flattened.into_iter().peekable());
        },

        | Some(TT::Literal(lit))
            if {
                s = lit.to_string();
                utils::extracted_string_literal(&mut s)
            }
        => match s.parse::<TokenStream>() {
            | Ok(ts) => ts,
            | Err(err) => return Err((
                lit.span(),
                format!("Invalid tokens: {}", err).into(),
            )),
        },

        | Some(TT::Ident(ident))
            if matches!(
                tokens.peek(),
                Some(TT::Punct(p)) if p.as_char() == '!'
            )
        => {
            drop(tokens.next());
            let group_contents = unwrap_next_token! {
                if let TT::Group(group) = tokens.next(), {
                    group.stream()
                } else {
                    failwith!("\
                        expected `{ ... }` or `( ... )` or `[ ... ]`\
                    ");
                }
            };
            match ident.to_string().as_str() {
                | "stringify" => group_contents,
                | "concat" => {
                    let mut ret = TokenStream::new();
                    let mut current = group_contents.into_iter().peekable();
                    loop {
                        let (parsed, mut remaining) = recurse(current)?;
                        ret.extend(parsed);
                        if remaining.peek().is_none() {
                            break ret;
                        }
                        unwrap_next_token! {
                            if let TT::Punct(p) = remaining.next(),
                                if (p.as_char() == ',')
                            {} else {
                                failwith!("expected nothing or `,`");
                            }
                        }
                        if remaining.peek().is_none() {
                            break ret;
                        }
                        current = remaining;
                    }
                },
                | _ => return Err((
                    ident.span(),
                    "expected `stringify` or `concat`".into(),
                )),
            }
        },

        | Some(bad_tt) => return Err((
            bad_tt.span(),
            err_msg!().into(),
        )),

        | None => return Err((
            Span::call_site(),
            concat!("Unexpected end of input: ", err_msg!()).into(),
        )),
    };
    (ret, tokens)
})}
