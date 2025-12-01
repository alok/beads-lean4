/-
Beads Documentation Generator
Builds HTML documentation using Verso Blog genre
-/
import VersoBlog
import Manual

open Verso Genre Blog Site Syntax

def theme : Theme := Theme.default

def beadsSite : Site := site Manual /
  .none

def main := blogMain theme beadsSite
