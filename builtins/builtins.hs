data Bool = True | False ;

data List a = Cons a (List a) | Nil ;

data Maybe a = Nothing | Just a ;

# len :: (List a) -> Integer ;
{#
len a = case a of {
    Nil -> 0;
    Cons _ tail -> 1 + len tail;
};
#}
