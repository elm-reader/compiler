module MultiFile1 exposing (fib10, foo)


foo : String
foo =
    "hello"


fibonacci n =
    if n <= 1 then
        if equalZero n then
            n
        else
            n
    else
        fibonacci (n - 1) + fibonacci (n - 2)


equalZero n =
    case n of
        0 ->
            True

        _ ->
            False


fib10 =
    fibonacci 10
