#!/bin/sed -f

1 d # delete first line of file
$ d # delete last line of file
s/>>>\(.*\)/$ `\1`  / # remove bird tracks, add spaces
s/\.\.\.\(.*\)/=> \1 /
s/^{-$/```/ # change lines containing only {- to ```
s/^-}$/```haskell/ # change lines containing only -} to ```haskell
/^```haskell$/ { # if the line conatains only ```haskell (so was a -} line
 N  # add next line to buffer
 /^```haskell\n{-|$/ d # and if the next line contains only {-|, delete it
}