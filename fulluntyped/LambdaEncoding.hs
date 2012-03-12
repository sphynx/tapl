{-
Encoding of booleans, numbers, lists and operations on them
using Haskell functions (as in lambda calculus).
-}

module LambdaEncoding where

import Control.Applicative

-- boolean constants (abbreviated true and false)
tru = \x y -> x   -- const
fls = \x y -> y   -- flip const

-- test = if, i.e. test c t e = if c then t else e
test = \c t e -> c t e   -- id

-- boolean functions
and' = \f s -> f s fls
or' = \f s -> f tru s
xor' = \f s -> f s (not' s)
not' = \g f s -> g s f

-- pairs
pair = \f s b -> b f s
fir = \p -> p tru
sec = \p -> p fls

-- numbers
zero = \s z -> z -- just like false!
one = \s z -> s z
two = \s z -> s (s z)
three = \s z -> s (s (s z))

-- arithmetic operations
scc = \n s z -> s (n s z)
-- scc = \n s z -> n s (s z)
-- scc = \n -> (.) <$> id <*> n
-- scc = \n -> (.) <$> n <*> id

-- mul = (.)
mul = \n1 n2 s z -> n1 (n2 s) z

-- pow = ($)
pow = \m n -> m n

-- add = liftA2 (.) -- led to strange types...
add = \m n s z -> m s (n s z)

num2int = \n -> n (+1) 0

is_zero = \n -> n (\x -> fls) tru

prd = \n -> fir (n ss zz)
  where
    zz = pair zero zero
    ss = \p -> pair (sec p) (scc (sec p))

-- FIXME: hm, 'sub x y' when x <= y is not typed properly here...
sub = \m n -> n prd m

-- lists

-- encoding: [a, b, d] = \c n -> c a (c b (c d n))
nil = \c n -> n -- aka false and zero
cons = \h t -> \c n -> c h (t c n)
is_nil = \xs -> xs (\x y -> fls) tru
hd = \xs -> xs (\h t -> h) fls
tl = \xs -> fir (xs cc nn)
  where
    nn = pair nil nil
    cc = \x p -> pair (sec p) (cons x (sec p))

-- list_sum = fix (\rec lst -> lst (\h t -> add h (rec t)) zero)

-- recursion (not possible to use as-is, since Haskell cannot construct
-- a type of Z-combinator from TAPL)
fix f = f (fix f)

to_list = \xs -> xs (:) []

-- test suite
check :: Bool
check = and
  [ test (and' tru tru) 1 0 == 1
  , test (and' tru fls) 1 0 == 0
  , test (and' fls tru) 1 0 == 0
  , test (and' fls fls) 1 0 == 0
  , test (or' tru tru) 1 0 == 1
  , test (or' tru fls) 1 0 == 1
  , test (or' fls tru) 1 0 == 1
  , test (or' fls fls) 1 0 == 0
  , test (xor' tru tru) 1 0 == 1
  , test (xor' tru fls) 1 0 == 0
  , test (xor' fls tru) 1 0 == 0
  , test (xor' fls fls) 1 0 == 1
  , test (not' tru) 1 0 == 0
  , test (not' fls) 1 0 == 1
  , test (fir $ pair tru fls) 1 0 == 1
  , test (sec $ pair tru fls) 1 0 == 0
  , num2int (add one two) == 3
  , num2int (add three zero) == 3
  , num2int (add three two) == 5
  , num2int (mul three two) == 6
  , num2int (mul three one) == 3
  , num2int (pow three two) == 8
  , num2int (pow three one) == 1
  , num2int (pow two three) == 9
  , test (is_zero two) 1 0 == 0
  , test (is_zero three) 1 0 == 0
  , test (is_zero zero) 1 0 == 1
  , test (is_zero (mul zero two)) 1 0 == 1
  , test (is_zero (prd one)) 1 0 == 1
  , num2int (prd two) == 1
  , num2int (prd (add one two)) == 2
  , num2int (prd (mul three two)) == 5
  , num2int (sub three one) == 2
  , num2int (sub two zero) == 2
  , num2int (sub one one) == 0
  , test (is_nil nil) 1 0 == 1
  , test (is_nil (cons one nil)) 1 0 == 0
  , test (is_zero (hd (cons two (cons one nil)))) 1 0 == 0
  , num2int (hd (cons three nil)) == 3
  , num2int (hd (tl (cons one (cons three nil)))) == 3
  , num2int (hd (tl (cons one (cons two nil)))) == 2
  ]
