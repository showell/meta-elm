**2019-11-05**

Welcome to **MetaElm**!

The code is [here](https://github.com/showell/MetaElm).  This is
my first announcement of the project, and it's currently a proof
of concept.

### MetaElm is Pure Elm

**MetaElm** is pure Elm code that implements a runtime for an
AST language that is **just Elm** running inside Elm.

It is sort of the same idea as [elm-in-elm](https://github.com/elm-in-elm/compiler),
but smaller and focused on literally running **inside** Elm.

The AST language is just an Elm custom type called `MeType.Expr`.

It lets you:

- use helpers like `MeList.initInts` or `MeParser.toExpr` to create AST values
- use helpers like `MeType.UserFunction` and `MeType.PipeLine` to create AST functions
- use `MeRunTime.computeExpr` to evaluate functions/expressions
- use `MeRepr.fromExpr` to serialize AST data to strings
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

* no preprocessor
* no postprocesor
* no shelling out to Haskell
* no shelling out to node.js
* no ports interacting with JS
* no hacking into Elm.Kernel
* no opcodes
* no custom-written list/dict types
* no parsing required (but there's a convenient JSON-like parser for values)

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
normalize : Context
normalize =
    let
        f =
            PipeLine
                (VarName "lst")
                [ MeList.indexedMap MeTuple.pair
                , MeList.sortByInt MeTuple.second
                , MeList.map MeTuple.first
                , MeList.indexedMap MeTuple.pair
                , MeList.sortByInt MeTuple.second
                , MeList.map MeTuple.first
                , MeList.map <| LambdaLeft "n" MeNumber.plus (MeInt.init 1)
                ]
    in
    [ ( "normalize", f ) ]
```

This AST is roughly equivalent to the Elm code below, but
we do **not** actually have a translation step.  The
above AST is interpreted directly by `computeExpr`.  The
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
    = VBool Bool
    | VInt Int
    | VFloat Float
    | VTuple ( Expr, Expr )
    | VList (List Expr)
    | VError String
```

This is still an early version, but here are the main data types that
are targeted for support:

- bools, numbers, and strings
- list, tuples, and dicts
- functions and operators
- records

Essentially, MetaElm targets basic primitives inside of
basic containers.  (I didn't list custom types above, since
they might be tricky, but I'm not ruling them out either.)

In MetaElm (as in Elm), everything is an expression.  But in
MetaElm everything literally is in a type called `Expr`.

##### Expr

Here's the current version (as of 2019-11-05):

```elm
type Expr
    = BinOp String FVV
    | Call String Context
    | ComputedFunc FV
    | ComputedValue V
    | F1 Expr Expr
    | F2 Expr Expr Expr
    | FuncCall Context String Context
    | IfElse Expr Expr Expr
    | Infix Expr Expr Expr
    | LambdaLeft String Expr Expr
    | LambdaRight Expr Expr String
    | LetIn (List ( String, Expr )) Expr
    | NamedFunc String FV
    | PipeLine Expr (List Expr)
    | SimpleValue V
    | Var String Expr
    | VarName String
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

Here is an example of `MeTuple.pair` wrapping
`Tuple.pair`:

```elm
-- MeTuple.elm

pair : Expr
pair =
    let
        pair1 : Expr -> FV
        pair1 left =
            \c rightExpr ->
                let
                    right =
                        compute c rightExpr
                in
                VTuple (Tuple.pair left right)
                    |> ComputedValue

        pair0 : FV
        pair0 c leftExpr =
            let
                left =
                    compute c leftExpr
            in
            pair1 left
                |> ComputedFunc
    in
    NamedFunc "Tuple.pair" pair0
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
`Expr`, so we just plop them into a tuple.

Here's a more complicated wrapper:

```elm
-- MeList.elm

map : Expr
map =
    let
        happyPath : Context -> List Expr -> FV -> Expr
        happyPath c lst mapper =
            lst
                |> List.map (mapper c)
                |> VList
                |> ComputedValue

        map1 : FV -> FV
        map1 mapper =
            \c lstExpr ->
                case getValue c lstExpr of
                    VList lst ->
                        happyPath c lst mapper

                    VError s ->
                        error ("bad list in map: " ++ s)

                    _ ->
                        error "need list in map"

        map0 : FV
        map0 c mapperExpr =
            let
                mapper =
                    getFuncV c mapperExpr
            in
            map1 mapper
                |> ComputedFunc
    in
    NamedFunc "List.map" map0
```

So the function is best understood as follows:
    
- MeList.map is an Expr wrapping map0
- compute (F1 meList.map mapper) wraps map1
- compute (F2 meList.map wrapper lst) calls happyPath

##### Discussion

Remember, once you enter Expr Space, everything goes back
to being pure Elm, but just a little bit abstracted.  Here's another
example of user-generated MetaElm:

``` elm
permuteFloats : Context
permuteFloats =
    let
        startList =
            PipeLine
                (VarName "lst")
                [ F1 MeList.map MeInt.toFloat
                ]

        newElements =
            PipeLine
                (VarName "startList")
                [ MeList.sortFloat
                , F1 MeList.map (LambdaLeft "n" MeNumber.plus (MeFloat.init 0.5))
                , LambdaRight (MeFloat.init 0.5) MeList.cons "items"
                ]

        f =
            LetIn
                [ ( "startList", startList )
                , ( "newElements", newElements )
                ]
                (PipeLine
                    (VarName "newElements")
                    [ F1 MeList.map MeList.singleton
                    , F1 MeList.map
                        (LambdaRight (VarName "startList") MeList.plus "x")
                    ]
                )
    in
    [ ( "permuteFloats", f ) ]
```

The above code doesn't require any special type checks.

And you can execute it from normal Elm Space using `compute`.

(For more context on the above example, see
[MeSnippet.elm](https://github.com/showell/MetaElm/blob/master/src/MeSnippet.elm).)

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

#### Execution model

Let's quickly review the prior few sections of this article

- you have value-like thingies that are `Expr`
- you have mapped versions of functions that are `Expr`

We have also briefly mentioned things like `UserFunction`.

In order to evaluate an expression (for example, turn 2+3
into 5), we need to call `MeRunTime.computeExpr`.

Then `computeExpr` just sets up an empty `context` for
`compute`:

```elm
computeExpr : Expr -> V
computeExpr expr =
    let
        context =
            Dict.empty
    in
    compute context expr
```

The `context` is just a dictionary of variables.  It's empty
by default, but it can be populated by when we evaluate
`FunctionCall` values.

The `compute` function is the heart of MetaElm, and it's
just a large `case` statement that works through all
the subtypes of `Expr`.

Many `Expr` subtypes are just values:

* ComputedValue, SimpleValue - trivial, just pull out values
* Var - pull out expression, recursively compute it
* VarName - look up context for expression, recursively compute it

The more interesting subtypes are expressions combining functions
and values

* PipeLine - maps roughly to `22 |> incr |> double`
* F1 - maps roughly to `someFunction 15`
* F2 - maps roughly to `someFunction 15 30`

Everything basically works on recursive descent, as you can
see from the code below:

```elm
-- MeRunTime.elm

evalPipeLine : Context -> Expr -> List Expr -> V
evalPipeLine context v lst =
    case lst of
        [] ->
            compute context v

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

- compute
- getFuncV
- evalPipeLine (itself)

The `getFuncV` function looks for function subtypes.  Examples
include:

- F1
- NamedFunc
- LambdaLeft
- LambdaRight

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
flesh out the core pieces like `compute` and
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
