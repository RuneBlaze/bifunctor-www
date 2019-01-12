---
layout: post
title:  "Bifunctors"
date:   2018-01-02 20:21:00
categories: 
---

{% marginnote 'mn-id-whatever' 'This is still an early draft version of the article.' %}Haskell bifunctors, compared to the ubiquitous functors, are much less known while still struturally significant. This article serves as a synthesis and a personal exploration on the topic of usefulness of bifunctors in Haskell.

Without much experience and background, I aim to avoid unwieldy analogies {%sidenote 'sn-id-monad-tutorial' 'Also known as the [monad tutorial fallacy](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/): analogies often fail to generalize.'%} and will paraphrase existing sources in hope of rigor. Bifunctors have served as a nice review to functors and enabled unique applications in datatypes. I wish this article, by no means comprehensive, will eventually convince you the same.

## Starting with Functors

Haskell functors, intuitively understood as mathematical covariant functors, usually act as the starting point towards the typeclass hierachy. While often leading to applicatives and monads, the somewhat "container-like" route, functors this time start a different path with ```Contravariant```, intuitively mathematical contravariant functors.

```haskell
class Contravariant f where
	contramap :: (b -> a) -> f a -> f b
```

As with ```fmap``` of functors, parametricity{% marginnote 'mn-id-parametricity' 'Also known as [free theorems](http://ttic.uchicago.edu/~dreyer/course/papers/wadler.pdf).' %} dictates the uniqueness of ```contramap``` for a given ADT. Intuitively, contravariant functors, different from functors, do not contain or produce value, but merely consume value, as seen in instances as wrappers of ```a -> Bool``` and ```a -> b```. The chemistry between covariant and contravariant functors might prove intriguing: covariant functors that are also contravariant, bivariant functors, are limited to phantoms containing no value, as evidenced by the ```coerce``` function{%sidenote 'sn-coerce' 'For an implementation and a more general context see [lens over tea #2](https://artyom.me/lens-over-tea-2).'%}:

```haskell
coerce :: (Functor f, Contravariant f) => f a -> f b
```

Curiously all covariant functors and contravariant functors are also invariant functors, defined by ```invmap	```. Trivially ```invmap``` can be derived from either  ```fmap``` or ```contramap```.

```haskell
class Invariant f where
	invmap :: (a -> b) -> (b -> a) -> f a -> f b
```

Even though ideally we would have ```class Invariant f => Functor f ```, in practicality few uses exists for invariant functors to justify that choice.{%sidenote 'sn-variance' 'See [Covariance, contravariance, and positive and negative position](https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance) for a more comprehensive treatment on this topic.'%}

## Towards Bifunctors

The functor instances for tuples might seem unnatural on first sight, as ```fmap``` over lists operates on all values but ```fmap``` on tuples can only operate on the second element, a fact trivially learned and understood when first approaching the functor typeclass.

```haskell
Prelude> (+1) <$> [1,2]
[2,3]
Prelude> (+1) <$> (1,2)
(1,3)
```

The sum type ```Either``` also displays similar behavior with regards to its functor instance, with ```Left``` values unaffected and ```Right``` values changed. Naturally the question of whether a functor instance can be defined with regarding to the first type argument (the type of the first element in tuples, and the type of the left element in sum types) arises, only achievable after introducing type wrappers. Also if the first type argument is functorial, can both functorial arguments be operated on at once? In our context, this motivates a function that can update both values of a tuple at once, and a function that upon meeting a sum type can either operate on the right or the left.

```haskell
mapTuple :: (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapEither :: (a -> x) -> (b -> y) -> Either a b -> Either x y
```

A quick observations yields that with  ```updateTuple``` given, setting the second argument to ```id``` would result in an updater only to the first component of the tuple, while setting the first argument to ```id``` degrades the updater to  ```fmap``` on tuples. The case is similar in ```updateEither```.

The above code, with the kind of  ```(,)``` and ```Either``` both as  ```* -> * -> *```, leads to the easy generalization of the core function among bifunctors, ```bimap```, with both tuples and ```Either``` as bifunctors.

```haskell
class Bifunctor p where
	bimap :: (a -> x) -> (b -> y) -> p a b -> p x y
```

Bifunctors{%sidenote 'sn-bifunctor' 'The [bifunctors](https://hackage.haskell.org/package/bifunctors-5) package includes definitions and common tools for bifunctors discussed here.'%} capture the essence of both of its type arguments as functorial and covariant, suggesting informally both of its type arguments can be used ```fmap``` upon simutaneously, as seen in ```bimap```, and individually, as demonstrated by its other functions ```first``` and ```second```.

```haskell
class Bifunctor p where
	first :: (a -> x) -> p a b -> p x b
	second :: (b -> y) -> p a b -> p a y
```

```bimap``` can be easily derived when having ```first``` and ```second```. In the reverse direction, ```bimap``` can degrade to ```first``` and ```second``` trivially. Thus only ```bimap``` or both ```first``` and ```second``` requires definition.

With ```first``` and ```second```, a generalized updater to both tuples and ```Either``` has been obtained, at least providing some immediate practical use. Wrappers to disguise bifunctors as functors, on either arguments, can be constructed with ease. Likewise, functors, through phantom types, can be lifted to bifunctors.{%sidenote 'sn-bifunctor-tools' 'These along with compositions can be found in the bifunctors package.'%}

Before diving deeper into bifunctors, its perhaps more popular counterpart, profunctors, should be mentioned. Profunctors are intuitively mathematical bifunctors that are contravariant in its first type argument while covariant in its second type argument{%sidenote 'sn-id-natural-ques' 'I still would like to know what bifunctors contravariant on both arguments are called.'%}. Informally a profunctor consumes its first type argument while produces or stores its second argument. Therefore profunctors are occasionally treated as generalized functions.

```haskell
class Profunctor p where
	dimap :: (x -> a) -> (b -> y) -> p a b -> p x y
	lmap :: (x -> a) -> p a b -> p x b
	rmap :: (b -> y) -> p a b -> p a y
```

Profunctors, along with profunctor optics{%sidenote 'sn-id-prof-opt' 'An overview of which could be found [here](https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/poptics.pdf).'%}, is another awesome topic that requires further exploration. From now on the topic would only focus on the bifunctor typeclass in Haskell.

## Laws and Properties

Bifunctor laws are close analogues of functor laws. From ```fmap id = id``` bifunctors have:

```haskell
bimap id id = id
first id = id
second id = id
```

Nonetheless a unique bifunctor law is also required, tying the three functions together.

```haskell
bimap f g = first f . second g
```

If these are ensured, by parametricity, the following laws, similar to the second functor law ```fmap (g . h) = (fmap g) . (fmap h)```, hold:

```haskell
bimap  (f . g) (h . i) = bimap f h . bimap g i
first  (f . g) = first  f . first  g
second (f . g) = second f . second g
```

Same as functors, by parametricity{%sidenote 'brent blog' 'The parametricity proof and other derivations of properties can be found in [Parametricity for Bifunctors](https://byorgey.wordpress.com/2018/03/30/parametricity-for-bifunctor/).'%}, for a given ADT only one unique bifunctor instance exists. With these laws a property, symmetric to ```bimap f g = first f . second g``` can be derived:

```haskell
bimap f g = second g . first f
```

The above property finishes the proof that ```first``` and ```second``` commutes.

```haskell
first f . second g = second g . first f
```

## Polynomial Functors and Bifunctors

The algebra of algebraic data types shows that the ```Bool``` type can be constructed as a sum of two unit types. Yet how to construct the ```Maybe``` type, as denoted in algebra by ```a + 1```, remains unclear. Adding a type parameter, thus allowing passing in variables such as ```a``` allows a functorial approach to encode these types. Most covariant functors can be broken down into a small set of primitive type operations using constants, identity, sum and product, constructing datatypes with genericity for free. {%sidenote 'crude-version' 'This can be seen as a crude precursor to [generic programming](https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/generic-programming.html).'%}

```haskell
data K a x = K a
data I x = I x
data S p q x = L (p x) | R (q x)
data P p q x = P (p x) (q x)
```

Using ```K``` we can encode constants, such as ```Zero``` or ```One``` lifted into functors.

```haskell
type Zero = K Void
type One = K ()
type Two = S One One
```

To rescind into reality, the two inhabitants of ```Two x```, with ```x``` fixed, can be constructed.

```haskell
t :: Two x
t = L (K ())

f :: Two x
f = R (K ())
```

With the building blocks ready, ```Maybe``` can be directly encoded as the sum of identity and 1.

```haskell
type Maybe = S I One

nothing :: Maybe x
nothing = R (K ())

just :: x -> Maybe x
just x = L (I x)
```

The representation here might look cumbersome, especially compared to the normal encoding of ```Maybe```, but it does affect our theory and can be mitigated by pattern synonynoms.

Notice that, as the basic building blocks, constant, identity, sum, and product are already functors, the functor instance for the ```Maybe``` type as defined above is automatically resolved to be a functor. The functor instances for `K`, `I`, `S`, and `P` are omitted, as they are straightforward and can be automatically derived by GHC.

A similar approach is taken when recovering `Either`, to recreate the algebraic form `a + b`, of two parameters. As more than one parameter determines the type now, the type cannot be encoded using polynomial functors, but instead requires the anagolous polynomial bifunctors.

```haskell
data K2 a x y = K2 a
data S2 p q x y = L2 (p x y) | R2 (q x y)
data P2 p q x y = P2 (p x y) (q x y)
```

As the identity `I` ceases to make sense here, two new bifunctors referencing the first argument and the second are defined.

```haskell
data Fst x y = Fst x
data Snd x y = Snd y
```

The polynomial bifunctors here also have uncomplicated instance definitions that are omitted, and can have those derived via the template haskell facilities exported by the `bifunctor` package. Perhaps unsurprisingly, the polynomial bifunctors have corresponding functor instances.

`Either` defined as `a + b` will be encoded as the sum of the first argument and the second, and the constructors prove to be obvious. Notice that the bifunctor and functor instances of `Either` here will be obtained for free.

```haskell
type Either = S2 Fst Snd

left :: a -> Either a b
left x = L2 (Fst x)

right :: b -> Either a b
right x = R2 (Snd x)
```

## Fixpoints on Polynomial Functors

To be written.

## Fixpoints on Polynomial Bifunctors

To be written.

## Bifunctors on Functor Fixpoints

To be written.

## Jokers, Clowns, and Dissections

To be written.