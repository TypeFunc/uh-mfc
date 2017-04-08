{- Here are some good blog posts on automatic differentiation
http://blog.sigfpe.com/2005/07/automatic-differentiation.html
http://augustss.blogspot.com/2007/04/overloading-haskell-numbers-part-2.html
-}

> module AD where

> import Numeric.AD
> import Numeric.AD.Jet

> doit x y z = do

You can compute derivatives of functions

> let result1 = diff sin 0 {- cos 0 -}

1.0

Or both the answer and the derivative of a function:

> let result2 = diff' (exp . log) 2

(2.0,1.0)

You can compute the derivative of a function with a constant parameter using auto:

> let t = 2.0 :: Double
> let result3 = diff (\ x -> auto t * sin x) 0

2.0

You can use a symbolic numeric type, like the one from simple-reflect to obtain symbolic derivatives:

> let result4 = diff atanh x

recip (1 - x * x) * 1

You can compute gradients for functions that take non-scalar values in the form of a Traversable functor full of AD variables.

> let result5 = grad (\[x,y,z] -> x * sin (x + log y)) [x,y,z]

[ 0 + (0 + sin (x + log y) * 1 + 1 * (0 + cos (x + log y) * (0 + x * 1)))
, 0 + (0 + recip y * (0 + 1 * (0 + cos (x + log y) * (0 + x * 1))))
, 0
]

which one can simplify to:

[ sin (x + log y) + cos (x + log y) * x, recip y * cos (x + log y) * x, 0 ]

If you need multiple derivatives you can calculate them with diffs:

> let result6 = take 10 $ diffs sin 1

[0.8414709848078965,0.5403023058681398,-0.8414709848078965,-0.5403023058681398,0.8414709848078965,0.5403023058681398,-0.8414709848078965,-0.5403023058681398,0.8414709848078965,0.5403023058681398]

or if your function takes multiple inputs, you can use grads, which returns an 'f-branching stream' of derivatives, that you can inspect lazily. Somewhat more intuitive answers can be obtained by converting the stream into the polymorphically recursive Jet data type. With that we can look at a single "layer" of the answer at a time:

The answer:

> let result6 = headJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

7.38905609893065

The gradient:

> let result7 = headJet $ tailJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

[14.7781121978613,7.38905609893065]

The hessian (n * n matrix of 2nd derivatives)

> let result8 = headJet $ tailJet $ tailJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

[[29.5562243957226,22.16716829679195],[22.16716829679195,7.38905609893065]]

Or even higher order tensors of derivatives as a jet.

> let result9 = headJet $ tailJet $ tailJet $ tailJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

[[[59.1124487914452,44.3343365935839],[44.3343365935839,14.7781121978613]],[[44.3343365935839,14.7781121978613],[14.7781121978613,7.38905609893065]]]

> return ()
