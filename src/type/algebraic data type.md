algebraic data type
======



   * `data cx => T u1 … uk = K1 t11 … t1k1 |  | Kn tn1 … tnkn`

   * data keyword defines a algebraic data type [HR 2010 syntax ref](http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-690004.2.1)

   * This declaration introduces a new type constructor T with zero or more constituent data constructors K1, …, Kn.

   * The type variables u1 through uk must be distinct and may appear in cx and the tij [ref](http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#dx10-69012)

   * Just as data values are built using data constructors,  type values are built from type constructors. (HR2010 P37)
this means, in the function type declaration, you can't supply a data constructor
f :: Just a -> String -- this is not correct
   * data type_constructor [type_variable] = value_constructor [parameter]
   * the names of types and values are independent of each other.
   * Not only is it legal for a value constructor to have the same name as its type constructor, it’s normal.
   * when data constructor used with field labels, it doesn't take type vars  [field labels](https://www.evernote.com/shard/s189/nl/21742161/f3ff6024-88de-4e81-83b8-acf286cd5e5c)



the declaration
  data Eq a => Set a = NilSet | ConsSet a (Set a)

type variable a is defined by Set a, the type variable may appear in constraints and data constructor.
introduces a type constructor Set of kind ∗→∗, and constructors NilSet and ConsSet with types
NilSet::  ∀ a.  Set  aConsSet::  ∀ a.  Eq   a  ⇒  a  →  Set   a  →  Set   a
In the example given, the overloaded type for ConsSet ensures that ConsSet can only be applied to values whose type is an instance of the class Eq.

the type of data constructor is Ki  ::  ∀ u1 … uk.  cxi  ⇒  ti1  →    →  tiki  →  (T u1 … uk)
which is a T not a K

Pattern matching against ConsSet also gives rise to an Eq a constraint. For example:


  f (ConsSet a s) = a
the function f has inferred type Eq a => Set a -> a. The context in the data declaration has no other effect whatsoever.
