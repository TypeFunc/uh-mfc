{- Here are some good blog posts on automatic differentiation
http://blog.sigfpe.com/2005/07/automatic-differentiation.html
http://augustss.blogspot.com/2007/04/overloading-haskell-numbers-part-2.html
-}

> module AD where
 
> import Numeric.AD
> import Numeric.AD.Jet
>
> import Debug.SimpleReflect.Vars
> import Debug.SimpleReflect.Expr

You can compute derivatives of functions

> result1 = diff sin 0 {- cos 0 -}

1.0

Or both the answer and the derivative of a function:

> result2 = diff' (exp . log) 2

(2.0,1.0)

You can compute the derivative of a function with a constant parameter using auto:

> period = 2.0 :: Double
> result3 = diff (\ x -> auto period * sin x) 0

2.0

You can use a symbolic numeric type, like the one from simple-reflect to obtain symbolic derivatives:

> result4 = diff atanh x

recip (1 - x * x) * 1

You can compute gradients for functions that take non-scalar values in the form of a Traversable functor full of AD variables.

> result5 = grad (\[x,y,z] -> x * sin (x + log y)) [x,y,z]

[ 0 + (0 + sin (x + log y) * 1 + 1 * (0 + cos (x + log y) * (0 + x * 1)))
, 0 + (0 + recip y * (0 + 1 * (0 + cos (x + log y) * (0 + x * 1))))
, 0
]

which one can simplify to:

[ sin (x + log y) + cos (x + log y) * x, recip y * cos (x + log y) * x, 0 ]

If you need multiple derivatives you can calculate them with diffs:

> result6 = take 10 $ diffs sin 1

[0.8414709848078965,0.5403023058681398,-0.8414709848078965,-0.5403023058681398,0.8414709848078965,0.5403023058681398,-0.8414709848078965,-0.5403023058681398,0.8414709848078965,0.5403023058681398]

or if your function takes multiple inputs, you can use grads, which returns an 'f-branching stream' of derivatives, that you can inspect lazily. Somewhat more intuitive answers can be obtained by converting the stream into the polymorphically recursive Jet data type. With that we can look at a single "layer" of the answer at a time:

The answer:

> result7 = headJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

7.38905609893065

The gradient:

> result8 = headJet $ tailJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

[14.7781121978613,7.38905609893065]

The hessian (n * n matrix of 2nd derivatives)

> result9 = headJet $ tailJet $ tailJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

[[29.5562243957226,22.16716829679195],[22.16716829679195,7.38905609893065]]

Or even higher order tensors of derivatives as a jet.

> result10 = headJet $ tailJet $ tailJet $ tailJet $ jet $  grads (\[x,y] -> exp (x * y)) [1,2]

[[[59.1124487914452,44.3343365935839],[44.3343365935839,14.7781121978613]],[[44.3343365935839,14.7781121978613],[14.7781121978613,7.38905609893065]]]


> sines n x = head $ drop n $ iterate sin x
> sines4 = sines 6 x
> nth_derivatives :: Int -> [Expr]
> nth_derivatives n = take n $ diffs (sines 6) x
