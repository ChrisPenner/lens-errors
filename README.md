# lens-errors

[HACKAGE](http://hackage.haskell.org/package/lens-errors)

Handling failure deep inside a lens chain intelligently is hard;
You tried to crawl deep into a tree, but it failed and you're not sure why!
This helps with that.

This library doesn't aim to serve every single use case; or provide a TON of 
error combinators, it mostly gives you the tools to write your own.

# Example
Here's what a usage could look like;

Let's say we have a nice tree:

```haskelll
let tree = Node 0 [Node 1 [Node 2 []]]
```

But we want to get the node at the bottom! No problem, we can write a traversal
for that!

```haskell
λ> tree ^.. branches . ix 0 . branches . ix 10 . root
[]
```

Oh; we failed! We can probably guess in this example why the failure occured,
but in larger more complex traversals it can be pretty tough to find sometimes;
and since traversals branch out we might even fail in more than one place at a time!

`lens-errors` gives us
a whole shwack of `fizzle` combinators which will allow us to effectively
'kill' a branch of the traversal with an error message while allowing other
branches to continue to limp along and see if they can succeed.

Why `fizzle` you ask? Words like `fail` or `throw` seem to strong, in this
case only certain branches of the traversal might fizzle out while allowing others to continue.

Let's build a new traversal which reports a nice error if we fail to index
into the child branch we want:

```haskell
let tryIx n = ix n `orFizzleWith` (\xs -> [show n <> " was out of bounds in list: " <> show xs])
```

Here we use `orFizzleWith` which will try to run the given traversal; but
will build and throw a nice error if the traversal returns no elements.

Now that we've added errors to our lens chain the standard lens-running combinators
like `^.` and `%~` won't work any more, but we've got new ones. Let's try `^&..`
which runs a traversal and collects the elements into a list, with nice errors along the way!

```haskell
>>> tree ^&.. branches . tryIx 0 . branches . tryIx 10 . root
(["10 was out of bounds in list: [Node {rootLabel = 2, subForest = []}]"],[])
```

What a great error message! Now we know exactly why we didn't manage to find any
elements there.

You'll notice we actually returned a tuple of type `([String], [Int])`; We return
the errors in the left half and values in the right. Because of the branching factor
of traversals, it's entirely possible for one branch to fail and another to succeed!

Let's see an example of that.

```haskell
>>> [1, 2, 3, 4] ^&.. traversed . fizzleWithUnless (\n -> show n <> " wasn't even!") even
("1 wasn't even!3 wasn't even!", [2, 4])
```

Here we see two interesting things! One is that it returned some elements successfully!
But also, it returned a funny looking error message; looks like our messages got smushed together!

`lens-error` lets you use any error type you want given you follow two conditions:

1. All lenses in the chain must agree on the same error type
2. The error type must be a Monoid.

The error combinators will end up mashing all your errors together, so usually
it's nice to just wrap them all in a list; up to you though!

There are a few more `fizzle` combinators available; hopefully you can build what you need with them :)

For instance we can include failure messages on prisms.

If we use `^&?`; We'll find the first successfull result; otherwise return all
the errors we encountered.

```haskell
>>> let prismError p name = p `orFizzleWith` (\v -> ["Value " <> show v <> " didn't match: " <> name])
>>> let _R = prismError _Right "_Right"

>>> [Left (1 :: Int), Left 2, Right (3 :: Int)] ^&? traversed . _R
Success 3
>>> [Left (1 :: Int), Left 2, Left 3] ^&? traversed . _R
Failure [ "Value Left 1 didn't match: _Right"
        , "Value Left 2 didn't match: _Right"
        , "Value Left 3 didn't match: _Right"]
```

# More examples

For more examples you can reference the [test suite](./test/Spec.hs).
