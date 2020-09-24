use ::unstringify::unstringify;

fn main ()
{
    unstringify!(r#"
        println!("What for?");
    "#);
    fn basic () { println!("Basic string literal"); }
    unstringify!(let $call = unstringify!("basic()") in {
        $call;
        $call;
    });
    unstringify!(let $tokens = unstringify!(r#"
        println!("Hello, World");
    "#) in {
        $tokens
        println!("{}", stringify!($tokens));
        $tokens
    });
    unstringify!(let $tokens = unstringify!(
        concat!(
            stringify!(Hello),
            concat!(stringify!(,), "World"),
            "!",
        )
    ) in {
        assert_eq!(
            stringify!($tokens),
            stringify!(Hello, World!),
        );
    });
    macro_rules! with_expr {(
        $expr:expr
    ) => (
        unstringify!(let $tokens = unstringify!($expr) in { $tokens })
    )}
    with_expr!(r#"println!("with_expr");"#);
}
