/-
Pure Graph Operations for Beads
Includes cycle detection for dependency graphs
-/
import Std.Data.HashSet
import Beads.Core.Issue

namespace Beads

/-- Edge in a dependency graph -/
structure Edge where
  from_ : IssueId
  to : IssueId
  deriving DecidableEq, Repr, Inhabited

namespace Edge
instance : BEq Edge := ⟨fun a b => a.from_ == b.from_ && a.to == b.to⟩
end Edge

/-- Simple dependency graph represented as edge list -/
structure Graph where
  edges : List Edge
  deriving Repr, Inhabited

namespace Graph

/-- Empty graph -/
def empty : Graph := { edges := [] }

/-- Add an edge to the graph -/
def addEdge (g : Graph) (from_ to : IssueId) : Graph :=
  { g with edges := { from_, to } :: g.edges }

/-- Get all vertices in the graph -/
def vertices (g : Graph) : List IssueId :=
  let froms := g.edges.map (·.from_)
  let tos := g.edges.map (·.to)
  (froms ++ tos).eraseDups

/-- Get outgoing neighbors of a vertex -/
def neighbors (g : Graph) (v : IssueId) : List IssueId :=
  g.edges.filter (·.from_ == v) |>.map (·.to)

/-- BFS to check if there's a path from source to target -/
def hasPath (g : Graph) (source target : IssueId) (fuel : Nat := 1000) : Bool :=
  bfsLoop g target [source] {} fuel
where
  bfsLoop (g : Graph) (target : IssueId) (queue : List IssueId)
      (visited : Std.HashSet IssueId) (fuel : Nat) : Bool :=
    if fuel == 0 then false  -- Give up if too many iterations
    else match queue with
    | [] => false
    | current :: rest =>
      if current == target then true
      else if visited.contains current then bfsLoop g target rest visited (fuel - 1)
      else
        let neighs := g.neighbors current
        bfsLoop g target (rest ++ neighs) (visited.insert current) (fuel - 1)
  termination_by fuel
  decreasing_by all_goals simp_all; omega

/-- Check if adding an edge would create a cycle
    A cycle would be created if there's already a path from 'to' back to 'from_' -/
def wouldCreateCycle (g : Graph) (from_ to : IssueId) : Bool :=
  -- Self-loop is always a cycle
  if from_ == to then true
  -- Check if there's a path from 'to' to 'from_' (would create cycle with new edge)
  else hasPath g to from_

/-- Check if the graph has any cycles using DFS -/
def hasCycle (g : Graph) : Bool :=
  let verts := g.vertices
  verts.any fun start =>
    dfsDetectCycle g start {} {} 1000
where
  dfsDetectCycle (g : Graph) (current : IssueId)
      (visited : Std.HashSet IssueId) (inStack : Std.HashSet IssueId)
      (fuel : Nat) : Bool :=
    if fuel == 0 then false
    else if inStack.contains current then true  -- Back edge = cycle
    else if visited.contains current then false  -- Already processed
    else
      let visited' := visited.insert current
      let inStack' := inStack.insert current
      let neighs := g.neighbors current
      neighs.any fun n => dfsDetectCycle g n visited' inStack' (fuel - 1)
  termination_by fuel
  decreasing_by all_goals simp_all; omega

/-- Convert a list of dependencies to a graph -/
def fromDependencies (deps : List Dependency) : Graph :=
  { edges := deps.map fun d => { from_ := d.issueId, to := d.dependsOnId } }

/-- Find all cycles in the graph (returns list of cycle paths) -/
def findCycles (g : Graph) (fuel : Nat := 100) : List (List IssueId) :=
  let verts := g.vertices
  findCyclesFrom g verts {} fuel
where
  findCyclesFrom (g : Graph) (remaining : List IssueId)
      (found : Std.HashSet (List IssueId)) (fuel : Nat) : List (List IssueId) :=
    if fuel == 0 then found.toList
    else match remaining with
    | [] => found.toList
    | v :: rest =>
      match dfsWithPath g v [v] {} (fuel * 10) with
      | some cycle =>
        let normalizedCycle := normalizeCycle cycle
        if found.contains normalizedCycle then
          findCyclesFrom g rest found (fuel - 1)
        else
          findCyclesFrom g rest (found.insert normalizedCycle) (fuel - 1)
      | none =>
        findCyclesFrom g rest found (fuel - 1)
  termination_by fuel
  decreasing_by all_goals simp_all; omega

  dfsWithPath (g : Graph) (current : IssueId) (path : List IssueId)
      (visited : Std.HashSet IssueId) (fuel : Nat) : Option (List IssueId) :=
    if fuel == 0 then none
    else
      let neighs := g.neighbors current
      neighs.findSome? fun n =>
        if path.contains n then
          -- Found a cycle - extract it
          some (path.dropWhile (· != n))
        else if visited.contains n then
          none
        else
          dfsWithPath g n (path ++ [n]) (visited.insert current) (fuel - 1)
  termination_by fuel
  decreasing_by all_goals simp_all; omega

  -- Normalize cycle to start with smallest element (for deduplication)
  normalizeCycle (cycle : List IssueId) : List IssueId :=
    -- Find minimum by comparing strings
    let minElem := cycle.foldl (fun acc x =>
      if x.value < acc.value then x else acc) (cycle.headD ⟨""⟩)
    -- Find index of minimum
    let rec findIdx (xs : List IssueId) (target : IssueId) (idx : Nat) : Nat :=
      match xs with
      | [] => 0
      | x :: rest => if x == target then idx else findIdx rest target (idx + 1)
    let idx := findIdx cycle minElem 0
    cycle.drop idx ++ cycle.take idx

end Graph

end Beads
