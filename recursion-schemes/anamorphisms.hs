data Term f = In { out :: f (Term f) }

-- arrows
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(f >>> g) x = g (f x)

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) = (.)

-- catamorphisms

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

-- The dual of a catamorphism is an anamorphism. It is a generalized "unfold".

type Coalgebra f a = a -> f a

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn