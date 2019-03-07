-- Working with types

{-# LANGUAGE ScopedTypeVariables #-}

-- The below is broken because the type system doesn't think the type b in apply
-- is the same b as in the type signature.
broken :: (a -> b) -> a -> b
broken f a = apply
    where
        apply :: b
        apply = f a

-- ScopedTypeVariables create a "type scope" when using forall a b.