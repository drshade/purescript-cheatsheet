# Cheatsheet

My handy scratch pad of knowledge around purescript. Mostly handy functions, type classes and operators - to help as a reminder.

# Functions

## module Data.Function
```hs
-- Basically ignore the second argument
const :: forall a b. a -> b -> a

-- Flip the first 2 arguments to a function
flip :: forall a b c. (a -> b -> c) -> b -> a -> c

-- Function application
apply :: forall a b. (a -> b) -> a -> b

-- Opposite
applyFlipped :: forall a b. a -> (a -> b) -> b

-- Call apply on it's argument multiple times (return type MUST match input type obviously now)
applyN :: forall a. (a -> a) -> Int -> a -> a
-- e.g. applyN (_ + 1) 5 0 --> 5

-- operators
infixr 0 apply as $
infixl 1 applyFlipped as #
```

# Type Classes

## Functor (module Data.Functor)

Run a function against a value captured inside a context, e.g. running a function expecting an `Int` against a `Maybe Int`, AND returning back the same context (`Maybe Int`).

```hs
class Functor f where
	map :: forall a b. (a -> b) -> f a -> f b

-- functions
-- ignore the value on the right, returning the left wrapped into the context
voidRight :: forall f a b. Functor f => a -> f b -> f a
-- and the opposite
voidLeft :: forall f a b. Functor f => a -> f b -> f b

-- apply a wrapped function to an unwrapped value, returning a wrapped result
flap :: forall a b f. Functor f => f (a -> b) -> a -> f b

-- operators
infixl 4 map as <$>
infixl 1 mapFlipped as <#>
infixl 4 voidRight as <$
infixl 4 voidLeft as $>
infixl 4 flap as <@>
```

e.g. 
```hs
-- for Maybe Int, map would be
map :: (Int -> Int) -> Maybe Int -> Maybe Int

map (+3) (Just 5) -- Just 8
map (+3) Nothing  -- Nothing

-- for Array String, map would be
map :: (Array String -> Array String) -> Array String -> Array String

-- Using the operator
(_ <> "bar") <$> ["foo", "power"] -- ["foobar", "powerbar"]

-- flap example
flap (Just (_ + 10)) 5 -- Just 15
Just (_ + 10) <@> 5    -- Same
```

## Apply (module Control.Apply)

Allows applying a function to a value under a context.

```hs
class Functor f <= Apply f where
	apply :: forall a b. f (a -> b) -> f a -> f b

-- functions
-- combine 2 computations, keeping only the result of the first
applyFirst :: forall a b f. Apply f => f a -> f b -> f a
applySecond :: forall a b f. Apply f => f a -> f b -> f b

-- Lift a function taking 2 arguments, returning a function taking those 
-- same arguments each under the new context
lift2 :: forall a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
lift3 :: ^^ same but with 1 extra arg
lift4 :: ^^ same but with 1 extra arg
lift5 :: ^^ same but with 1 extra arg

-- operators
infixl 4 apply as <*>
infixl 4 applyFirst as <*
infixl 4 applySecond as *>
```

```hs
-- concating a Maybe String using apply directly
-- notice that the function (_ <> "BAR") must itself be a Maybe (ie same context) to
-- type check against `f (a -> b)` in apply
apply (Just (_ <> "BAR")) (Just "FOO")	-- Just "FOOBAR"

-- apply a function to elements of an array
-- again, notice the function must itself be wrapped within the Array context
apply [(_ + 10)] [1, 2, 3]           -- [11, 12, 13]
apply [(_ + 10), (_ + 20)] [1, 2, 3] -- [11, 12, 13, 21, 22, 23]

-- Same examples using operator <*>
Just (_ <> "BAR") <*> Just "FOO"
[(_ + 10)] <*> [1, 2, 3]
```

## Applicative (module Control.Applicative)

Extends `Apply` with a function called `pure` which can lift a single value into the context.

```hs
class Apply f <= Applicative f where
	pure :: forall a f. a -> f a

liftA1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
```

e.g.
```hs
-- examples
liftA1 (\e -> e + 1) (Just 1)	-- Just 2
```

## Bind (module Control.Bind)

Extends `Apply` with the `bind` function (>>=) to compose computations in sequence, using return value of each as input into the next.

```hs
class (Apply m) <= Bind m where
	bind :: forall a b. m a -> (a -> m b) -> m b

-- functions
-- simply flip around the arguments to bind
bindFlipped :: forall a b. (a -> m b) -> m a -> m b
-- join two applications of a context into one
join :: Bind m => m (m a) -> m a

-- kleisli compose - compose two computations together into a single one
kleisliCompose :: Bind m => (a -> m b) -> (b -> m c) -> a -> m c
kleisliComposeFlipped :: Bind m => (b -> m c) -> (a -> m b) -> a -> m c

-- if expression, based on condition holding
ifM :: forall a m. Bind m => m Boolean -> m a -> m a -> m a

-- operators
infixl 1 bind as >>=
infixr 1 bindFlipped as =<<
infixr 1 composeKleisli as >=>
infixr 1 composeKleisliFlipped as <=<
```

e.g.
```hs
-- bind examples
bind ["bat", "cat"] (\e -> [e <> "man"]) -- ["batman","catman"]
["bat", "cat"] >>= (\e -> [e <> "man"])  -- Same
(\e -> [e <> "man"]) =<< ["bat", "cat"]  -- Same, using bind flipped

-- kleisli compose examples, basically just composing the functions provided to bind
bind ["bat", "cat"] $ (\e -> [e <> "man"]) >=> (\e -> [e <> "guy"]) -- ["batmanguy","catmanguy"]

-- examples of join
-- sort of like a flatten - convert an `Array (Array Int)` to `Array Int`
join [[1, 2, 3]] -- [1, 2, 3]

-- similar for Maybe (Maybe String)
join (Just (Just "Hello")) -- Just "Hello"

-- example of ifM
ifM [false] [1, 2] [2, 3] -- [2, 3]
```

## Semigroup (module Data.Semigroup)

Semigroup really represents the ability to concatenate elements, e.g. lists, arrays, strings. i.e. supporting the `<>` operator.

```hs
class Semigroup a where
	append :: a -> a -> a

-- operators
infixr 5 append as <>
```

e.g. 
```hs
-- Pretty obvious
"hello" <> "world"
[1, 2] <> [3, 4]
```

## Semiring (module Data.Semiring)

Semiring represents the ability to add, multiply as well as provide a zero and one element.

```hs
class Semiring a where
	add :: a -> a -> a
	mul :: a -> a -> a
	zero :: a
	one :: a

-- operators
infixl 6 add as +
infixl 7 mul as *
```

## Monad (module Control.Monad)

Combines the `Applicative` and `Bind` type classes, providing an ability to lift values into the context through `pure` as well as composition through `bind`.

```hs
class (Applicative m, Bind m) <= Monad m where
	-- nothing extra

-- functions
-- a default version of <$> (`map` from Functor) for any `Monad` you are defining - because we now have `pure` available to us in this context
liftM1 :: forall m a b. Monad m => (a -> b) -> m a -> m b
-- a default version of <*> (`apply` from Apply) for any `Monad` you are defining - for the same reasons as above
ap :: forall m a b. Monad m => m (a -> b) -> m a -> m b

-- Conditional action based on boolean
whenM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
unlessM :: forall m. Monad m => m Boolean -> m Unit -> m Unit
```

## Monoid (module Data.Monoid)

Simply a `Semigroup`, but with a representation of empty called `mempty`

```hs
class Semigroup m <= Monoid m where
	mempty :: m

-- functions
-- append an element to itself multiple times
power :: forall m. Monoid m => m -> Int -> m

-- "truncate" a monoid to it's mempty value based on a condition
guard :: forall m. Monoid m => Boolean -> m -> m
```

e.g.
```hs
-- power examples
power [5] 4       -- [5,5,5,5]
power "ducks! " 4 -- "ducks! ducks! ducks! ducks! "

-- guard example
guard false [1,2,3] -- []
```

# Handy Algebraic Data Types

## Effect (module Effect)

Effect wraps an effectful computation holding a Type. e.g. `Effect String` means a side-effecting computation returning a `String`. 

```hs
data Effect :: Type -> Type
```

Functions for working with Effects

```hs
-- Keep executing an Effect until the return value is True
untilE :: Effect Boolean -> Effect Unit

-- While the first computation returns true, execute the second in loop
whileE :: forall a. Effect Boolean -> Effect a -> Effect Unit

-- Run a computation a number of times, (low to high]
forE :: Int -> Int -> (Int -> Effect Unit) -> Effect Unit

-- Run a computation a number of times, based on array element
foreachE :: Array a -> (a -> Effect Unit) -> Effect Unit
```

# Just a collection of all the operators
```hs
--
-- class Functor
--
infixl 4 map as <$>
infixl 1 mapFlipped as <#>

-- These two are interesting
-- voidRight :: forall f a b. Functor f => a -> f b -> f a
-- i.e. ignore the value on the right, returning the left wrapped into the context
infixl 4 voidRight as <$
-- and the opposite with voidLeft
infixl 4 voidLeft as $>
-- not sure yet where this might be useful

--
-- class Apply
--
infixl 4 apply as <*>
infixl 4 applyFirst as <*
infixl 4 applySecond as *>
```
