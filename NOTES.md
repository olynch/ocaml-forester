I have a compiler that features multiple compilation phases. For examples sake,
consider the following simplified pipeline:

```ocaml
module M = Map.Make(String)

type state = {
  phase1: string M.t;
  phase2: int M.t;
  phase3: string M.t;
}

let step1 = M.map String.length
let step2 = M.map Int.to_string

let init source =
  {
    phase1 = source;
    phase2 = M.empty;
    phase3 = M.empty
  }

let run_phase1
    : state -> state
  = fun state ->
    {
      state with
      phase2 = step1 state.phase1
    }

let run_phase2
    : state -> state
  = fun state ->
    {
      state with
      phase3 = step2 state.phase2
    }
```

*Why do you need a state type? Can't we just compose the functions step1 and
step2?* 

The answer is yes, if we only want to consider batch compilation. The
state type arose from the development of a language server that needs access to
all compilation phases. I also want to cleverly update the state upon granular
changes (for example, when a source changes only recompile the stuff that
depends on it, although the example is too simplified to capture that)

This gets to the type of thing I would like to capture. 

*Can I use GADTs to prevent phase2 from being run before phase1 completed?*

I am imagining that `state` could be a GADT such that the types of `run_phase*`
reflect that a phase transition is happening. I have had success with GADTs in
the past and it feels to me like this is a potential usecase for them, but I
can't seemt to actually write down what I want.

I realize that I might be trying to be too clever. I could just design an
interface that ensures that everything is being used correctly, but using GADTs
is more slick.

Any pointers, suggestions or alternative approaches are appreciated!

Related:
- https://raphael-proust.gitlab.io/code/gadt-tips-and-tricks.html
