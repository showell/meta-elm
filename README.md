# meta-elm

Elm runtime on top of Elm, running in pure Elm

# Description

This project allows you to write Elm code in a
DSL that:

- can be evaluated inside Elm
- can be serialized to simpler Elm code

It's kinda like elm-in-elm, but it's smaller and
targeted specifically at running Elm natively in Elm.

# Example usage

See
[MeSnippet.elm](https://github.com/showell/meta-elm/blob/master/src/MeSnippet.elm)
for examples of how to use meta-elm.

# Project status

This is still early stages.  The functionality presented here is fairly
well tested, but there is lots more work to do, particularly when it comes
to wrapping things like List/Dict/etc.

Also, I haven't spent a ton of effort on documentation yet.

