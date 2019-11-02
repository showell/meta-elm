**2019-11-01**

Welcome to **MetaElm**!

The code is [here](https://github.com/showell/MetaElm).  This is
my first announcement of the project, and it's currently a proof
of concept.

### MetaElm is Pure Elm

**MetaElm** is pure Elm code that implements a runtime for an
AST language that is **just Elm** running inside Elm. Think
of it as Elm-in-Elm.

The AST language is just an Elm custom type called `MeType.Expr`.

It lets you:

- use helpers like `MeList.initInts` to create AST values
- use helpers like `MeType.UserFunction` and `MeType.PipeLine` to create AST functions
- use `MeRunTime.computeVal` to evaluate functions/expressions
- use `MeRepr.fromVal` and `MeRepr.fromExpr` to serialize AST data to strings
- use `MeList.toList` and friends to convert wrapped types back to core types.
- use `MeElmCode.toElmCode` to generate "normal" Elm code

You may be interested in using MetaElm for the following
(somewhat overlapping) use cases:

* embed Elm examples into books and online tutorials
* learn about simple runtime systems
* generate multiple variations of Elm code
* convert MetaElm ASTs to other targets
* code-generate things like JSON decoders
* build online REPLs
* build refactoring tools or debuggers

It has the following features:

* it's extendible
* it integrates seamlessly into "normal" Elm programs
* it wraps actual Elm core code for heaving lifting (e.g. `List.sortBy`)
* it's tiny (~1k LOC as of now)
* it's **just Elm** (really!)

Let me elaborate on the last point.  This system is **just Elm**.  There
are very few moving parts:

* no parser
* no preprocessor
* no postprocesor
* no shelling out to Haskell
* no shelling out to node.js
* no ports interacting with JS
* no hacking into Elm.Kernel
* no opcodes
* no custom-written list/dict types

It's really **just Elm**.

To be fair, even though you never leave Elm to write MetaElm,
you will work in two separate conceptual spaces:

- Elm Space - this is normal Elm with normal Int/List/Tuple/etc.
- Expr Space - this is MetaElm, where everything is an Expr

Let's not dive too deep into theory yet, though.  Let's see it
in action.

### Example

Here is an example of the AST (i.e. MetaElm or Expr Space).  Note
that the code looks like Elm, because it is **just Elm**.

```elm
let
    one =
        MeInt.init 1

    incr =
        MeType.Curry MeNumber.plus one

    lst =
        MeType.VarName "lst"

    pipeline =
        MeType.PipeLine
            lst
            [ MeList.indexedMap MeTuple.pair
            , MeList.sortByInt MeTuple.second
            , MeList.map MeTuple.first
            , MeList.indexedMap MeTuple.pair
            , MeList.sortByInt MeTuple.second
            , MeList.map MeTuple.first
            , MeList.map incr
            ]

    f =
        MeType.UserFunction "normalize" [ "lst" ] pipeline
in
MeType.FunctionCall f
    [ ( "lst", testList )
    ]
```

This AST is roughly equivalent to the Elm code below, but
we do **not** actually have a translation step.  The
above AST is interpreted directly by `computeVal`.  The
below code is just a valid Elm representation of the AST (which
you can produce using `toElmCode` for educational use cases).

```elm
normalize lst =
    lst
        |> List.indexedMap Tuple.pair
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.indexedMap Tuple.pair
        |> List.sortBy Tuple.second
        |> List.map Tuple.first
        |> List.map (\n -> n + 1)

normalize testList
```

Let's talk about the architecture a bit...

### Architecture

The runtime system is comprised of types, wrapped
functions, and an execution model.

When you write MetaElm, you are really just writing Elm.
But you do have two modes...

#### Elm Space vs. Expr Space

In Elm Space, everything is like the normal Elm you're
used to.

In Expr Space, it's still **just Elm**, but you operate
on the AST (aka MetaElm), and the AST nodes are all just
values of the union type `Expr`.

Let's dive deeper on types...

#### Types

The main thing that drives the AST is the custom type `V`,
which looks like this:

```elm
type V
    = VInt Int
    | VFloat Float
    | VTuple ( Expr, Expr )
    | VList (List Expr)
    | VError String
```

This is still an early version, but here are the main data types that
are targeted for support:

- numbers and strings
- list, tuples, and dicts
- functions and operators
- records

Essentially, MetaElm targets basic primitives inside of
basic containers.  (The one conspicuos omission here is custom
types.  They may be tricky; I just haven't thought hard about
them yet.  Everything else "has a plan".)

In MetaElm (as in Elm), everything is an expression.  But in
MetaElm everything literally is in a type called `Expr`.

##### Expr

My current version of the `Expr` type is pretty small.

Here's the current version (as of 2019-11-01):

```elm
type Expr
    = SimpleValue V
    | ComputedValue V
    | VarName String
    | Var String Expr
    | UserFunction String (List String) Expr
    | FunctionCall Expr Context
    | FunctionV String FV
    | ComposeF String Expr FV
    | FunctionVV String FVV
    | PipeLine Expr (List Expr)
    | Curry Expr Expr
```

The `Expr` type **is** the AST.  You can either evaluate
the AST or render it to strings/code.

The mapping between `V`/`Expr` and Elm "native" types
is straightforward...

##### Mapping values

Here is an example mapping of `[21, 42]` to an `Expr`:

```elm
[21, 42] |> MeList.initInts

-- above is the same as
SimpleValue (VList [ SimpleValue (VInt 21), SimpleValue (VInt 42) ])
```

The provided mapping helpers should be easy to understand:

```elm
-- MeList
initInts : List Int -> Expr
initInts nums =
    nums
        |> List.map MeInt.init
        |> VList
        |> SimpleValue

-- MeInt
init : Int -> Expr
init num =
    num
        |> VInt
        |> SimpleValue
```

So `MeList.inits` doesn't return a `List Int`.  Instead,
each element is converted into a `SimpleValue`, then put
into an actual `List`, which in turn is wrapped into a
`SimpleValue`.

Everything is still isomorphic in some sense. For any `f`
we can construct a wrapper functions `f'` such that...

```elm
someVal |> f == someVal |> someInboundMappingHelper |> f' |> someOutboundMappingHelper
```

Also, once you have functions `f'`, `g'`, and `h'` that work in
"`Expr` space", you can stay in `Expr` space and compose
and apply `f'`/`g'`/`h'` in a natural fashion.

But we do need wrapper functions to interact with
core Elm things like `List`...

#### Wrapped Functions

Everything in MetaElm is either a value or a function. MetaElm
is **just Elm**, so everything that looks like a function is
executed as a function.  In other words, there is no low level
machinery like a VM that steps through op codes, or whatever
else you might imagine.

Instead, functions like `List.map` are just wrapped inside
functions like `MeList.map`.  The wrapper functions allow
MetaElm to fit into Elm's type system, because they have
general types like
`Context -> Expr -> V` or `Context -> Expr -> Expr -V`.

The generalized types allow us to walk the AST and compose
functions without generating compiler errors.  (Remember,
there's no `Any` type in Elm, so we kinda fake it.)

##### Examples

Here is a simple example of `MeTuple.pair` wrapping
`Tuple.pair`:

```elm
-- MeTuple.elm

pair : Expr
pair =
    let
        f : FVV
        f _ expr1 expr2 =
            VTuple (Tuple.pair expr1 expr2)
    in
    FunctionVV "Tuple.pair" f
```

Note that `MeTuple.pair` is just a function, because
MetaElm is **just Elm**.  But what it actually returns
is more like a representation of a function, which we'll
apply when we walk the AST.  But inside we have `f`, which
is truly a function in the most prosaic sense.

`Tuple.pair` is kind of a simple example, because we
don't need to do any
runtime checking of types.  Every `MeTuple` is of type
(`Expr`, `Expr`), and everything we'd pass it is an
`Expr`, so we just plop them into a tuple.  (Note that
we could say ` (expr1, expr2) ` instead of
`Tuple.pair expr1 expr2` here, but the code example
emphasizes the wrapper nature.

Here's a more complicated wrapper:

```elm
-- MeList.elm

map : Expr -> Expr
map mapperExpr =
    let
        happy_path : List Expr -> (Expr -> V) -> V
        happy_path lst mapper =
            lst
                |> List.map mapper
                |> List.map ComputedValue
                |> VList

        f c expr =
            case ( computeV c expr, getFuncV c mapperExpr ) of
                ( VList lst, Ok mapper ) ->
                    happy_path lst (mapper c)

                ( _, Err s ) ->
                    VError ("bad mapper" ++ s)

                _ ->
                    VError "map wants a list"
    in
    ComposeF "List.map" mapperExpr f
```

So the above example is kind of gnarly, but don't panic yet.

##### Discussion

Remember, once you enter Expr Space, everything goes back
to being pure Elm, but just a little bit abstracted.  Here's another
example of user-generated MetaElm:

``` elm
permuteFloats : MeType.Expr -> MeType.Expr
permuteFloats testList =
    let
        lst =
            MeType.VarName "lst"

        pipeline =
            MeType.PipeLine
                lst
                [ MeList.map MeInt.toFloat
                , MeList.map (MeType.Curry MeNumber.plus (MeFloat.init 0.5))
                ]

        f =
            MeType.UserFunction "permuteFloats" [ "lst" ] pipeline
    in
    MeType.FunctionCall f
        [ ( "lst", testList )
        ]
```

The above code doesn't require any custom type checks.

And you can execute from normal Elm Space using `computeVal`.

(For more context on the above example, see
[MeExample.elm](https://github.com/showell/MetaElm/blob/master/src/MeExample.elm).)

##### Wrapped functions in repo

Another reason not to panic about wrapper functions is that you get
many out of the box.  (I should point out that this announcement is at
the early stages of the project, so there are many more wrappers to be
written, but the goal here is to have a repository of wrapper functions.)

You can find wrapper functions in the
[src](https://github.com/showell/MetaElm/tree/master/src)
directory of the project.  Examples are:

- MeInt
- MeFloat
- MeNumber
- MeList
- MeTuple

I hope to have MeString and MeDict versions soon.  And, of course,
anyone can contribute extensions here.

##### Structure

Let's focus more specifically on `MeList.map` from a structural
standpoint.  Here is the code again (with some details obviously
omitted):


```elm
-- MeList.elm

map : Expr -> Expr
map mapperExpr =
    let
        -- some details omitted for brevity

        f c expr =
            case -- ...
                ( VList lst, Ok mapper ) ->
                    happy_path lst (mapper c)

                -- ...
    in
    ComposeF "List.map" mapperExpr f
```

When you call `MeList.map` you actually get back an `Expr`:

```elm
ComposeF "List.map" mapperExpr f
```

And then the `f` that actually wraps the **actual** `List.map`
is "inside" the `ComposeF` value.

So how we do actually execute `f`?

This is a segue into the execution model...


#### Execution model

Let's quickly review the prior few sections of this article

- you have value-like thingies that are `Expr`
- you have mapped versions of functions that are `Expr`

We have also briefly mentioned things like `UserFunction`.

In order to evaluate an expression (for example, turn 2+3
into 5), we need to call `MeRunTime.computeVal`.

Then `computeVal` just sets up an empty `context` for
`computeV`:

```elm
computeVal : Expr -> V
computeVal expr =
    let
        context =
            []
    in
    computeV context expr
```

The `context` is just a list of variables.  It's empty
by default, but it can be populated by when we evaluate
`FunctionCall` values.

The `computeV` function is the heart of MetaElm, and it's
just a large `case` statement that works through all
the subtypes of `Expr`.

Many `Expr` subtypes are just values:

* ComputedValue, SimpleValue - trivial, just pull out values
* Var - pull out expression, recursively compute it
* VarName - look up context for expression, recursively compute it

The more interesting subtypes are expressions combining functions
and values

* PipeLine - an example is `22 |> incr |> double`
* FunctionCall - an example is `someFunction 15 18`

Everything basically works on recursive descent, as you can
see from the code below:

```elm
-- MeRunTime.elm

evalPipeLine : Context -> Expr -> List Expr -> V
evalPipeLine context v lst =
    case lst of
        [] ->
            computeV context v

        head :: rest ->
            case getFuncV context head of
                Ok f ->
                    let
                        newV =
                            f context v
                    in
                    evalPipeLine context (ComputedValue newV) rest

                Err s ->
                    VError ("wanted function in pipeline: " ++ s)
```

You can see that `evalPipeLine` calls three other functions:

- computeV
- getFuncV
- evalPipeLine (itself)

The `getFuncV` function looks for function subtypes.  Examples
include:

- FunctionV
- ComposeF
- Curry

You'll recall from earlier that `MeList.map` returns this:

```elm
ComposeF "List.map" mapperExpr f
```

And we asked when does `f` actually get called?  The answer is
that functions like `evalPipeLine` call them.

### Conclusion

Thank you for reading this far!

This project is very much a work in progress, and this article
is its first public announcement.

I feel like I've written enough code to explain the core
concept of what I'm trying to achieve here, but I haven't
articulated my overall vision for the project.  That will
have to wait for a second article.  You can find the code
in my [MetaElm repo](https://github.com/showell/MetaElm).

My immediate goals are to just translate some more real
Elm code into MetaElm snippets, and that will help me
flesh out the core pieces like `computeVal` and
other stuff in `MeRunTime`.

There is also the somewhat tedious work of writing wrappers.
I will probably automate that somewhat in the future, but
I'm still kind of experimenting with the core pieces and
evolving the basic design.  I feel 90% there with the design.
I went down some very wrong paths before this design, but
the design that's there now seems workable.

One of the interesting twists to this whole endeavor is
that MetaElm could be made more "dynamic" than Elm.  I'm
not sure that's a worthwhile path to explore; it's just
a curious observation. (It also explains the need for the
somewhat clunky wrapper functions.)

I've written enough for now, and I will appreciate any
feedback from this first announcement.

Thanks!

-- @showell (find me on Elm Slack)
-- Steve Howell
