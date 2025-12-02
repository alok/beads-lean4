/-
Cycle Detection Property Tests
Tests for graph cycle detection algorithms
-/
import BeadsTest.Arbitrary
import Plausible.Testable
import Beads.Core.Graph

open Plausible
open Beads

-- ============================================================================
-- Graph Construction Helpers
-- ============================================================================

/-- Build a graph from a list of edge pairs -/
def buildGraph (edges : List (IssueId × IssueId)) : Graph :=
  edges.foldl (fun g (f, t) => g.addEdge f t) Graph.empty

/-- Create IssueId from string -/
def mkId (s : String) : IssueId := ⟨s⟩

-- ============================================================================
-- Basic Cycle Detection Properties
-- ============================================================================

-- Self-loop is always a cycle
def selfLoopIsCycle (id : IssueId) : Bool :=
  let g := Graph.empty.addEdge id id
  g.hasCycle && g.wouldCreateCycle id id

-- Empty graph has no cycles
def emptyGraphNoCycle : Bool :=
  !Graph.empty.hasCycle

-- Single edge A→B has no cycle
def singleEdgeNoCycle (a b : IssueId) : Bool :=
  if a == b then true  -- Skip self-loops
  else !((Graph.empty.addEdge a b).hasCycle)

-- Two-node cycle: A→B, B→A
def twoNodeCycleDetected (a b : IssueId) : Bool :=
  if a == b then true  -- Skip if same
  else
    let g := Graph.empty.addEdge a b
    g.wouldCreateCycle b a

-- Three-node cycle: A→B→C→A
def threeNodeCycleDetected : Bool :=
  let a := mkId "bd-aaa"
  let b := mkId "bd-bbb"
  let c := mkId "bd-ccc"
  let g := buildGraph [(a, b), (b, c)]
  g.wouldCreateCycle c a

-- Linear chain has no cycle
def linearChainNoCycle (ids : List IssueId) : Bool :=
  if ids.length < 2 then true
  else
    let uniqueIds := ids.eraseDups
    if uniqueIds.length < 2 then true
    else
      let edges := uniqueIds.zip (uniqueIds.drop 1)
      let g := buildGraph edges
      !g.hasCycle

-- Adding edge that doesn't close a path doesn't create cycle
def nonClosingEdgeNoCycle (a b c : IssueId) : Bool :=
  if a == b || b == c || a == c then true  -- Need distinct nodes
  else
    let g := Graph.empty.addEdge a b
    !g.wouldCreateCycle a c  -- A→C doesn't close B→?→A

-- ============================================================================
-- wouldCreateCycle Properties
-- ============================================================================

-- wouldCreateCycle is reflexive for same node
def wouldCreateCycleReflexive (id : IssueId) : Bool :=
  Graph.empty.wouldCreateCycle id id

-- If A→B exists, wouldCreateCycle(B, A) should be true
def wouldCreateCycleDetectsReverse (a b : IssueId) : Bool :=
  if a == b then true
  else
    let g := Graph.empty.addEdge a b
    g.wouldCreateCycle b a

-- Transitive: A→B→C implies wouldCreateCycle(C, A)
def wouldCreateCycleTransitive (a b c : IssueId) : Bool :=
  if a == b || b == c || a == c then true
  else
    let g := buildGraph [(a, b), (b, c)]
    g.wouldCreateCycle c a

-- ============================================================================
-- hasPath Properties
-- ============================================================================

-- Node has path to itself in a cycle
def pathToSelfInCycle : Bool :=
  let a := mkId "bd-aaa"
  let b := mkId "bd-bbb"
  let g := buildGraph [(a, b), (b, a)]
  g.hasPath a a

-- Direct edge means path exists
def directEdgeHasPath (a b : IssueId) : Bool :=
  if a == b then true
  else
    let g := Graph.empty.addEdge a b
    g.hasPath a b

-- Transitive path: A→B→C means A has path to C
def transitivePathExists (a b c : IssueId) : Bool :=
  if a == b || b == c || a == c then true
  else
    let g := buildGraph [(a, b), (b, c)]
    g.hasPath a c

-- No path in reverse direction for acyclic graph
def noReversePathAcyclic (a b : IssueId) : Bool :=
  if a == b then true
  else
    let g := Graph.empty.addEdge a b
    !g.hasPath b a

-- ============================================================================
-- Graph Structure Properties
-- ============================================================================

-- Adding edge increases edge count
def addEdgeIncreasesCount (g : Graph) (a b : IssueId) : Bool :=
  (g.addEdge a b).edges.length == g.edges.length + 1

-- Vertices include all edge endpoints
def verticesIncludeEndpoints (a b : IssueId) : Bool :=
  let g := Graph.empty.addEdge a b
  let verts := g.vertices
  verts.contains a && verts.contains b

-- Neighbors returns correct edges
def neighborsCorrect (a b c : IssueId) : Bool :=
  if a == b || b == c || a == c then true
  else
    let g := buildGraph [(a, b), (a, c)]
    let neighs := g.neighbors a
    neighs.contains b && neighs.contains c && !neighs.contains a

-- ============================================================================
-- Dependency Conversion Properties
-- ============================================================================

-- fromDependencies preserves edge count
def fromDepsPreservesCount (deps : List Dependency) : Bool :=
  let g := Graph.fromDependencies deps
  g.edges.length == deps.length

-- fromDependencies maps issueId to from_ and dependsOnId to to
def fromDepsCorrectMapping (dep : Dependency) : Bool :=
  let g := Graph.fromDependencies [dep]
  match g.edges with
  | [e] => e.from_ == dep.issueId && e.to == dep.dependsOnId
  | _ => false

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Basic cycle detection
#eval Testable.check (∀ id : IssueId, selfLoopIsCycle id)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check emptyGraphNoCycle
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check (∀ a b : IssueId, singleEdgeNoCycle a b)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check (∀ a b : IssueId, twoNodeCycleDetected a b)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check threeNodeCycleDetected
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check (∀ ids : List IssueId, linearChainNoCycle ids)
  (cfg := { numInst := 30, quiet := true })

#eval Testable.check (∀ a b c : IssueId, nonClosingEdgeNoCycle a b c)
  (cfg := { numInst := 30, quiet := true })

-- wouldCreateCycle properties
#eval Testable.check (∀ id : IssueId, wouldCreateCycleReflexive id)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check (∀ a b : IssueId, wouldCreateCycleDetectsReverse a b)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check (∀ a b c : IssueId, wouldCreateCycleTransitive a b c)
  (cfg := { numInst := 30, quiet := true })

-- hasPath properties
#eval Testable.check pathToSelfInCycle
  (cfg := { numInst := 1, quiet := true })

#eval Testable.check (∀ a b : IssueId, directEdgeHasPath a b)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check (∀ a b c : IssueId, transitivePathExists a b c)
  (cfg := { numInst := 30, quiet := true })

#eval Testable.check (∀ a b : IssueId, noReversePathAcyclic a b)
  (cfg := { numInst := 50, quiet := true })

-- Graph structure properties
#eval Testable.check (∀ a b : IssueId, addEdgeIncreasesCount Graph.empty a b)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check (∀ a b : IssueId, verticesIncludeEndpoints a b)
  (cfg := { numInst := 50, quiet := true })

#eval Testable.check (∀ a b c : IssueId, neighborsCorrect a b c)
  (cfg := { numInst := 30, quiet := true })

-- Dependency conversion
#eval Testable.check (∀ deps : List Dependency, fromDepsPreservesCount deps)
  (cfg := { numInst := 30, quiet := true })

#eval Testable.check (∀ dep : Dependency, fromDepsCorrectMapping dep)
  (cfg := { numInst := 50, quiet := true })

