3.0.0
-----

- completed all wrappers for List.elm
- cleaned up support for F1/F2/F3/F4/F5 and A1/A2/A3/A4/A5
- removed support for LambdaLeft/LambdaRight (just use F1/Infix)
- removed support for Function (just use F1/F2/etc.)
- removed MeElmCode (just use MeCodeGen)
- replaced BinOp with OpFunc
- made sort/sortBy generic (and removed sortByInt, etc.)
- made cons/append official names for ::/++
- added MeInt.modBy
- added VMaybe

2.0.0
-----

- add examples
- eval/emit list/tuples more aggressively
- rename F1/F2 to A1/A2

1.1.0
-----
This version could do basic operations on lists, tuples,
etc.  I publicized it a bit, but I don't believe anybody
was using it.  There are no release notes for this or any
other earlier release.
