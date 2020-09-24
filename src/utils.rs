/// mini-`syn`
macro_rules! unwrap_next_token {(
    if let $($pat:pat)|+ = $mb_token:expr,
        $(if ($($guard:tt)*))?
        $expr:block
    else {
        failwith!($err_msg:expr $(, $($($rest:tt)+)?)? );
    }
) => (
    match $mb_token {
        $( | Some($pat) )+
        $(if $($guard)*)?
        => $expr,
        | Some(bad_token) => return Err((
            bad_token.span(),
            {
                #[allow(unused_macros)]
                macro_rules! it {() => ($err_msg)}
                {
                    $($(
                        macro_rules! it {() => (
                            format!($err_msg, $($rest)+)
                        )}
                    )?)?
                    it!().into()
                }
            },
        )),
        | None => return Err((
            Span::call_site(),
            {
                #[allow(unused_macros)]
                macro_rules! it {() => (
                    concat!("Unexpected end of input: ", $err_msg)
                )}
                {
                    $($(
                        macro_rules! it {() => (
                            format!(
                                concat!("Unexpected end of input: ", $err_msg),
                                $($rest)+
                            )
                        )}
                    )?)?
                    it!().into()
                }
            },
        ))
    }
)}

/// mini-`quote`
macro_rules! ts {
    ( ($($input:tt)*) ) => (
        Group::new(Delimiter::Parenthesis, ts![$($input)*])
    );
    ( [$($input:tt)*] ) => (
        Group::new(Delimiter::Bracket, ts![$($input)*])
    );
    ( {$($input:tt)*} ) => (
        Group::new(Delimiter::Brace, ts![$($input)*])
    );

    (
        $($expr:expr),* $(,)?
    ) => (
        <TokenStream as ::core::iter::FromIterator<TT>>::from_iter(vec![
            $($expr.into() ,)*
        ])
    );
}

pub(in crate)
fn extracted_string_literal (
    s: &'_ mut String,
) -> bool
{
    for (i, c) in s.char_indices() {
        match c {
            | 'r' | '#' => continue,
            | '"' => {
                if i == 0 {
                    // No `r`: remove ending double-quote (`"`)
                    s.truncate(s.len() - 1);
                } else {
                    // One `r`: remove `i - 1` pounds (`#`), and ending double-quote
                    s.truncate(s.len() - i);
                }
                drop(s.drain(..= i));
                return true;
            },
            | _ => return false,
        }
    }
    false
}
