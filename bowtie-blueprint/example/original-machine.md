Examples of wrappers, and product types, and functions.

Note that the original design was:

type Point = Point
  { x : Rational
  , y : Rational
  }

foldr : ∀ a. ∀ b. (a -> b -> b) -> b -> List a -> b

foldl : ∀ a. ∀ b. (b -> a -> b) -> b -> List a -> b

```
type UserName = UserName Text

type Point = Point Rational Rational

foldr : (a -> b -> b) -> b -> List a -> b

foldl : (b -> a -> b) -> b -> List a -> b
```

Examples of sum types.

TODO: allow putting each constructor on its own line

```
type Picture = Line Point Point | Circle Point Rational

type Output = OutputChar Char | OutputLine Text | OutputPicture Picture

type Input = InputChar Char | InputLine Text

type Machine a = I (Input -> Machine a) | O Output (Machine a) | Done a | Exit

type Machine = Step Picture (Input -> Machine) | Done | Exit
```
