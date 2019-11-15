# meta-elm

Elm runtime on top of Elm, running in pure Elm

# Description

This project allows you to write Elm code in a
DSL that:

- can be evaluated inside Elm
- can be serialized to simpler Elm code

It can be used with an Elm-native parser, such as elm-in-elm,
to create REPLs and other learning tools.  Thie DSL will
always represent a subset of Elm, because we need to wrap
native methods and don't support every syntax.

# Example usage

See
[MeSnippet.elm](src/MeSnippet.elm)
for examples of how to use meta-elm.

# Project status

This is still early stages.  The functionality presented here is fairly
well tested, but there is lots more work to do, particularly when it comes
to wrapping things like List/Dict/etc.

Also, I haven't spent a ton of effort on documentation yet.

You can read the [release notes](ReleaseNotes.md) to see recent changes.

