-- Terms, types, and kinds

-- the kind system in Haskell is sort of the "type system for types."
-- In Haskell, the kind of types is * (or in later GHC versions, Type)

-- However, kinds apply to everything on the type-level, even though they might
-- not be "types". 

-- example) Show a => a -> String. Does Show a have a kind? Yeah! It's a constraint kind.
-- the kind of Show a is `Constraint`.

-- exercise) What is the kind of Show? A: Type -> Constraint
-- exercise) What of Functor? A: (Type -> Type) -> Constraint

-- The language extension DataKinds allow us to create our own kinds.

{-# LANGUAGE DataKinds #-}

kind Bool = 'True | 'False