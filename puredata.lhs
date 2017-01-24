\documentclass[a4paper]{article}
\setlength{\parskip}{\baselineskip}%
\usepackage[margin=3cm]{geometry}
%include lhs2TeX.fmt
\begin{document}

\title{Interpreter modelling the semantics of Pure Data}
\author{Hisham Muhammad}

\maketitle{}

This is an interpreter designed to model the core semantics of Pure Data,
a programmable music synthesis application, which features a dataflow
language at its core. 

The intention here is to describe the evaluation logic of the language,
so the goal here is clarity, not performance.

This implementation uses only standard modules included in the Haskell Platform:

\begin{code}
import Data.Sequence (Seq, fromList, index, update)
import qualified Data.Sequence (length, foldlWithIndex)
import Data.Foldable (foldl', toList)
import qualified Data.Foldable (foldl)
import Data.List (sort)
import Text.Printf
import Data.Fixed
\end{code}

\section{Representation of programs}

A Pure Data program is represented with the \emph{PdPatch} data type in our model,
which contains the DSP buffer size, a sequence of nodes, a sequence of
connections between nodes, and the pre-computed topological sort of audio
connections (stored as a list of integer indices).

\begin{code}
data PdPatch = PdPatch {
                  bufferSize  :: Int,
                  nodes       :: Seq PdNode,
                  conns       :: Seq PdConnection,
                  dspSort     :: [Int]
               }
\end{code}

Atoms are the primitive values in Pure Data: they can be numbers, strings
(called "symbols") or opaque pointers. Opaque pointers are used by graphical
objects only, which we will not represent here.

\begin{code}

data PdAtom  =  PdFloat     Double
             |  PdSymbol    String
   deriving Eq

instance Show PdAtom where
   show (PdFloat f)     = show f
   show (PdSymbol s)    = show s

\end{code}

Nodes may be objects, atom boxes or message boxes. In Pure Data, objects are
initialized via ``creation arguments'': a string of arguments, represented here
as a list of atoms. We also store in an object its number of inlets and
outlets.

\begin{code}

data PdNode  =  PdObject      [PdAtom] Int Int
             |  PdAtomBox     [PdAtom]
             |  PdMessageBox  [PdCommand]
   deriving Show

\end{code}

Message boxes contain commands written in the textual sub-language of Pure
Data. Here, we represent it not as a string, but as a parsed command line,
consisting of a receiver and a list of tokens (which may be literal values or
numbered references to inlet data (written in $\$n$ in the textual
language). Note that a single message box may contain a number of messages.

\begin{code}

data PdCommand = PdCommand PdReceiver [PdToken]
   deriving Show

data PdReceiver  =  PdToOutlet
                 |  PdReceiver String
                 |  PdRDollar Int
                 |  PdReceiverErr
   deriving Show

data PdToken  =  PdTDollar  Int
              |  PdTAtom    PdAtom
   deriving Show

\end{code}

Finally, we represent the connections of the graph as a sequence of adjacency
pairs, where each pair is represented as a \emph{PdConnection}, itself composed
of two pairs: the node index and outlet index for the source, and the node
index and inlet index for the destination. 

\begin{code}

data PdConnection = PdConnection (Int, Int) (Int, Int)
   deriving Show

\end{code}

\section{Representation of state}

The representation of a state in our interpreter is a structure containing the
following values: the step count, which will double as our timestamp, since
Pure Data has time-based execution; the state for each node; and the output,
which is the state of the Pd logger window (\emph{not} the audio output).

The state for each node, on its turn, contains a sequence of atom buffers,
one for each inlet, and an internal memory (represented as a list of atoms).
Memory consumption during execution is therefore variable, characterizing
a dynamic dataflow model.

\begin{code}

data PdState = PdState Int (Seq PdNodeState) [String]

data PdNodeState = PdNodeState (Seq [PdAtom]) [PdAtom]

instance Show PdState where
   show (PdState step nss output) =
      "Step: "  ++ show step
                ++ " - Nodes: " 
                ++ show (toList nss) 
                ++ " - Output: {\n" 
                ++ concatMap (\l -> l ++ "\n") output 
                ++ "}\n"

instance Show PdNodeState where
   show (PdNodeState inlets atoms) =
      "{" ++ show (toList inlets) ++ " | " ++ (show atoms) ++ "}\n"

\end{code}

\section{Execution}

The execution mode of Pure Data is data-driven. The user triggers events via
its interface, and those events cause a cascading series of firings. The user
may trigger events by clicking nodes, entering numbers (or using MIDI devices,
which are equivalent to entering numbers).

We represent events with a timestamp, the node index indicating which node was
triggered, and an optional atom representing data entered by the user.

\begin{code}

data PdEvent = PdEvent Int Int (Maybe PdAtom)
   deriving Show

\end{code}

\subsection{Main loop}

The execution of the interpreter, therefore, is a loop of evaluation steps.
The main driver function takes a number of steps, the program to run,
a list of timed events, accumulating a list states. We are interested
in all states, not only the final one, because we want to be able to
inspect the results of the execution over time.

Note that the program itself, {\emph patch}, remains unchanged over time. This
is typical of a language with liveness level 2: the program cannot be modified
during execution.

\begin{code}

runSteps :: Int -> PdPatch -> [PdEvent] -> [PdState]
runSteps nSteps patch events =
   reverse $ snd $ foldl' acc (events, [initialState patch]) [0 .. (nSteps - 1)]
   where
      acc :: ([PdEvent], [PdState]) -> Int -> ([PdEvent], [PdState])
      acc (events, states@(s : ss)) step =
         (nextEvents, s' : states)
         where
            (currEvents, nextEvents) = span (\(PdEvent ts _ _) -> ts == step) events
            s' = runStep patch s currEvents

\end{code}

The loop above extracts the sublist of relevant events for the current
timestamp, and hands it over to the main evaluation function, \emph{runStep},
which, given a patch, the current state, and a list of events, produces
a new state.

This function processes events and the DSP tree. Following the specified
semantics of Pure Data, this happens in an alternating fashion: all pending
messages are handled, and then the entire DSP tree is processed.

\begin{code}

runStep :: PdPatch -> PdState -> [PdEvent] -> PdState
runStep patch s@(PdState step _ _) events =
   let
      s'@(PdState _ nss output) = foldr (runEvent patch) s events
      s''@(PdState _ nss' output') =
         if step `mod` 2 == 0
         then runDspTree patch s'
         else s'
   in
      PdState (step + 1) nss' output'

\end{code}

In our model, the DSP tree are processed at half the rate of events (hence,
\emph{runDspTree} is called at every other run of \emph{runStep}).
Assuming that a step in our interpreter is 1~ms, this means the DSP engine
runs once every 2~ms (the default configuration of Pd runs the engine every
1.45~ms; with a 64-sample buffer, this amounts to an audio sample rate of
44,100~Hz — with this simplification in our interpreter, we get 36,000~Hz).

For handling state updates, we will use a couple of utility functions that
produce a new state, so we introduce them here. Function \emph{setInlet}
update the contents of an inlet's queue; \emph{setNodeState} updates the whole
state of a node.

\begin{code}

setInlet :: (Int, Int) -> [PdAtom] -> PdState -> PdState
setInlet (dst, inl) atoms state@(PdState _ nss _) =
   let
      ns@(PdNodeState inlets internal) = index nss dst
      ns' = PdNodeState (update inl atoms inlets) internal
   in
      setNodeState dst ns' state

setNodeState :: Int -> PdNodeState -> PdState -> PdState
setNodeState nidx ns state@(PdState step nss output) =
   PdState step (update nidx ns nss) output

\end{code}

\subsection{Event processing}

Two kinds of events can be triggered by the user. Message boxes may be
clicked, processing all commands stored inside them, or new numeric values
may be entered into atom boxes.

Updating an atom box causes the new value to be fired to its outlet. We do it
producing a synthetic command (\emph{atomCmd}) when handling events on
\emph{PdAtomBox} objects. \emph{PdMessageBox} may contain multiple commands
(which are semicolon-separated in the textual syntax), so we fold over that
list running commands.

\begin{code}

runEvent :: PdPatch -> PdEvent -> PdState -> PdState
runEvent patch event@(PdEvent ts nidx arg) state =
   setInlet (nidx, 0) inletData state'
      where
         node = index (nodes patch) nidx
         (inletData, state') = case node of
            PdMessageBox cmds ->
               ([], foldl' (runCommand patch nidx) state cmds)
            PdAtomBox _ ->
               case arg of
                  Just atom ->
                     let atomCmd = PdCommand PdToOutlet [PdTAtom atom]
                     in ([atom], runCommand patch nidx state atomCmd)
                  Nothing ->
                     error "Events on atom boxes expect an argument."
            _ -> ([], state)

\end{code}

Pure Data commands are written in its textual language. Commands may include
references to data obtained via inlets of the node using the $\$n$ notation.
For example, sending \texttt{10 20} to a message box containing \texttt{pitch \$2 velocity \$1}
connected to an object box \texttt{print} will print to the log window the string
\texttt{pitch 20 velocity 10}.

We run a given command \emph{cmd} on a node (with index \emph{nidx})
by first obtaining the inlet data currently stored in the node state.
Then we perform $\$$-expansion on the command's tokens. Then, based on the
receiver of the message, we route it through the graph (forwarding it to every
outlet, in a classic dataflow fashion) or symbolically, sending it to all
objects configured as a receivers for the given name.

\begin{code}

runCommand :: PdPatch -> Int -> PdState -> PdCommand -> PdState
runCommand patch nidx state@(PdState _ nss _) cmd =
   let
      (PdNodeState inlets _) = index nss nidx
      inletData = index inlets 0
      (recv, atoms) = dollarExpansion cmd inletData
   in
      case recv of
         PdToOutlet ->
            forEachOutlet patch nidx (sendMessage patch atoms) state
         PdReceiver r ->
            forEachReceiver patch r (sendMessage patch atoms) state
         PdReceiverErr ->
            printOut [PdSymbol "$1: symbol needed as message destination"] state

\end{code}

The process of $\$$-expansion is a simple substitution, where the receiver must
be a string. Invalid indices are converted to zero. (In Pure Data, they also
produce an error message to the log window, but here we omit this for brevity.)
We also handle here a syntactic shortcut: messages with numbers like \texttt{1.0}
expand to \texttt{float 1.0}.

\begin{code}

ffor a f = fmap f a

dollarExpansion :: PdCommand -> [PdAtom] -> (PdReceiver, [PdAtom])
dollarExpansion cmd@(PdCommand recv tokens) inlData =
   (recv', atoms')
   where
      inlAt n = if n < length inlData then inlData !! n else PdFloat 0
      recv' =
         case recv of
            PdRDollar n ->
               case inlAt n of
                  PdSymbol s  -> PdReceiver s
                  _           -> PdReceiverErr
            _ -> recv
      atoms' = 
         normalize $ ffor tokens (\token ->
            case token of
               PdTDollar n   -> inlAt n
               PdTAtom atom  -> atom
         )
      normalize atoms@(PdFloat f : xs)  = (PdSymbol "float" : atoms)
      normalize atoms                   = atoms

\end{code}

When sending a command, we propagate it to every connected outlet of a node.
This function takes an operation (a curried version of \emph{sendMessage},
which bundles the message) and folds over the list of connections of the
patch, applying the operation over all matching connections. In Section~\ref{messages}
we will present the implementation of \emph{sendMessage}, which handles
the various objects supported by this interpreter.

\begin{code}

forEachOutlet :: PdPatch  -> Int 
                          -> (PdNode -> (Int, Int) -> PdState -> PdState) 
                          -> PdState -> PdState
forEachOutlet patch nidx op state =
   Data.Foldable.foldl handle state (conns patch)
   where
      handle :: PdState -> PdConnection -> PdState
      handle state (PdConnection (src, _) (dst, inl))
         | src == nidx  = op (index (nodes patch) dst) (dst, inl) state
         | otherwise    = state

\end{code}

Indirect connections are handled similarly. Instead of folding over the list
of connections, we fold over the list of nodes, looking for objects declared
as \texttt{receive $name$}. Note that the search happens over the
statically-declared list of nodes of the patch. While it is possible construct
a message at runtime and determine the receiver dynamically, it is not
possible to change the identifier of a \texttt{receive} node at runtime.

\begin{code}

forEachReceiver :: PdPatch  -> String 
                            -> (PdNode -> (Int, Int) -> PdState -> PdState) 
                            -> PdState -> PdState
forEachReceiver patch name op state =
   Data.Sequence.foldlWithIndex handle state (nodes patch)
   where
      handle :: PdState -> Int -> PdNode -> PdState
      handle state dst node@(PdObject (PdSymbol "receive" : (PdSymbol rname : _)) _ _)
         | name == rname = forEachOutlet patch dst op state
      handle state _ _ = state

\end{code}

\subsection{Audio processing}

The processing of audio nodes is very different from that of message nodes.
Before execution, the audio nodes are topologically sorted, producing an
order according to which they are evaluated on each DSP update. For simplicity,
we do not compute this order at the beginning of execution, and merely assume
it is given as an input (in the \emph{dspSort} field of \emph{patch}).

As the list of nodes is traversed, each object is triggered (applying the
\emph{performDsp} function) and then the new computed value of its audio
buffer is propagated to the inlets of the nodes to which they are connected. 

\begin{code}

runDspTree :: PdPatch -> PdState -> PdState
runDspTree patch state@(PdState step nss output) =
   PdState step newNss output
   where
      newNss = foldl handle (zeroDspInlets nss (dspSort patch)) (dspSort patch)
      handle :: (Seq PdNodeState) -> Int -> (Seq PdNodeState)
      handle nss nidx =
         let
            obj = index (nodes patch) nidx
            ns@(PdNodeState inlets internal) = index nss nidx
            (outDsp, newInternal) = performDsp obj ns
            nss'' = update nidx (PdNodeState inlets newInternal) nss
         in
            Data.Foldable.foldl (propagate outDsp) nss'' (conns patch)
            where
               propagate :: [[PdAtom]] -> (Seq PdNodeState) -> PdConnection -> (Seq PdNodeState)
               propagate outDsp nss (PdConnection (src, outl) (dst, inl))
                  | src == nidx  = addToInlet (dst, inl) (outDsp !! outl) nss
                  | otherwise    = nss

\end{code}

Each audio node has a 64-sample buffer that needs to be cleared before each
traversal. Note that this is different from handling inlets in message
objects: for message objects, the inlets become empty once consumed. Here, we
need the inlet buffers to be filled with zeros.

\begin{code}

zeroDspInlets :: (Seq PdNodeState) -> [Int] -> (Seq PdNodeState)
zeroDspInlets nss dspSort =
   fromList $ clearedStates 0 (toList nss) (sort dspSort)
      where
         zeroInlets :: Int -> (Seq [PdAtom])
         zeroInlets n = fromList $ replicate n (replicate 64 (PdFloat 0.0))

         zeroState :: PdNodeState -> PdNodeState
         zeroState (PdNodeState inlets internal) =
            PdNodeState (zeroInlets (Data.Sequence.length inlets)) internal

         clearedStates :: Int -> [PdNodeState] -> [Int] -> [PdNodeState]
         clearedStates i (st:sts) indices@(nidx:idxs)
            | i == nidx  = zeroState st  : clearedStates (i+1) sts idxs
            | otherwise  = st            : clearedStates (i+1) sts indices
         clearedStates i  nss  []  = nss
         clearedStates i  []   _   = []

\end{code}

The reason why we fill the inlets with zeros is because when multiple
nodes connect to the same inlet in a DSP object, additive synthesis
is performed: the values of the incoming buffer are added to the
current contents of the inlet buffer, subject to saturation (audio
values are internally floats between -1.0 and 1.0).

\begin{code}

addToInlet :: (Int, Int) -> [PdAtom] -> (Seq PdNodeState) -> (Seq PdNodeState)
addToInlet (dst, inl) atoms nss = update dst ns' nss
   where
      saturate (PdFloat f) = PdFloat (max (-1.0) (min 1.0 f))
      satSum (PdFloat a, PdFloat b) = saturate $ PdFloat (a + b)
      ns@(PdNodeState inlets internal) = index nss dst
      oldAtoms = index inlets inl
      newAtoms = fmap satSum (zip oldAtoms atoms)
      ns' = PdNodeState (update inl newAtoms inlets) internal

\end{code}

In Section~\ref{dsps} we will present \emph{performDsp}, which implements the
various DSP objects supported by this interpreter.

\subsection{Initial state}

Finally, for completeness of the execution model, we present here the
functions that create the initial state.

\begin{code}

emptyInlets :: Int -> Seq [PdAtom]
emptyInlets n = fromList (replicate n [])

initialState :: PdPatch -> PdState
initialState (PdPatch _ nodes _ _) = PdState 0 (fmap emptyNode nodes) []
   where 
   emptyNode node =
      case node of
         PdAtomBox     atoms    -> PdNodeState (emptyInlets 1)    atoms
         PdObject      _ inl _  -> PdNodeState (emptyInlets inl)  []
         PdMessageBox  _        -> PdNodeState (emptyInlets 1)    []
      
\end{code}

\section{Language constructs}

The graphical language of Pure Data is graph-based and contains only nodes and edges.
The contents of nodes (object boxes, message boxes and atom boxes) are textual.
Like there are two kinds of edges (message and audio), there are also two kinds of
objects. Audio-handling objects are identified by a $~$ suffix in their names
(the Pure Data documentation calls them "tilde objects". In our interpreter,
plain objects are implemented in the \emph{sendMessage} function (Section~\ref{messages})
and tilde objects are implemented in the \emph{performDsp} function (Section~\ref{dsps}).

For printing to the log, we present a simple auxiliary function that adds to the
output log of the state value.

\begin{code}

printOut :: [PdAtom] -> PdState -> PdState
printOut atoms state@(PdState step nss output) =
   PdState step nss (output ++ [show atoms])

\end{code}

\subsection{Plain objects}
\label{messages}

The implementation of all non-audio nodes is done in the \emph{sendMessage}
function, which pattern-matches on the structure of the node (which includes
the parsed representation of its textual definition). 

\begin{code}

sendMessage :: PdPatch -> [PdAtom] -> PdNode -> (Int, Int) -> PdState -> PdState

\end{code}

The function takes as arguments the patch, the input arguments received via
the inlets (a list of atoms), the node to be matched, the node-inlet pair through
which the atoms arrived, and the current state.

It produces a new state, that is the result of the complete propagation of the
message. It is up to the node to decide whether it will fire data through its
outlets.

\begin{code}

-- message box:

sendMessage patch atoms (PdMessageBox cmds) (dst, 0) state =
   foldl (runCommand patch dst) (setInlet (dst, 0) atoms state) cmds

-- "print" object:

sendMessage patch (PdSymbol "float" : fs) (PdObject (PdSymbol "print" : xs) _ _) (_, 0) state =
   printOut ([PdSymbol "print: "] ++ xs ++ fs) state

sendMessage patch atoms (PdObject (PdSymbol "print" : xs) _ _) (_, 0) state =
   printOut ([PdSymbol "print: "] ++ xs ++ atoms) state

-- "float" object:

sendMessage patch [PdSymbol "bang"] (PdObject (PdSymbol "float" : xs) _ _) (dst, 0) state@(PdState _ nss _) =
   let
      (PdNodeState inlets internal) = index nss dst
      oldInternal = if internal /= [] then internal else [PdFloat 0]
      inlet1 = index inlets 1
      newInternal = if inlet1 == [] then oldInternal else inlet1
      state' = setNodeState dst (PdNodeState (emptyInlets (Data.Sequence.length inlets)) newInternal) state
   in
      forEachOutlet patch dst (sendMessage patch (PdSymbol "float" : newInternal)) state'

sendMessage patch (PdSymbol "float" : fl) (PdObject [PdSymbol "float"] inl _) (dst, 0) state@(PdState _ nss _) =
   let
      state' = setNodeState dst (PdNodeState (emptyInlets inl) fl) state
   in
      forEachOutlet patch dst (sendMessage patch (PdSymbol "float" : fl)) state'

-- "+" object:

sendMessage patch [PdSymbol "float", fl] (PdObject [PdSymbol "+", n] _ _) (dst, 0) state@(PdState _ nss _) =
   let
      (PdNodeState inlets internal) = 
         index nss dst
      (PdFloat val0) = fl
      inlet1 = index inlets 1
      (PdFloat val1) = if inlet1 == [] then n else head inlet1
      newInternal = [PdFloat (val0 + val1)]
      state' = setNodeState dst (PdNodeState (update 0 [fl] inlets) newInternal) state
   in
      forEachOutlet patch dst (sendMessage patch (PdSymbol "float" : newInternal)) state'

sendMessage patch [PdSymbol "bang"] (PdObject [PdSymbol "+", n] _ _) (dst, 0) state@(PdState _ nss _) =
   let
      (PdNodeState inlets internal) = index nss dst
      inlet0 = index inlets 0
      (PdFloat val0) = if inlet0 == [] then (PdFloat 0) else head inlet0
      inlet1 = index inlets 1
      (PdFloat val1) = if inlet1 == [] then n else head inlet1
      newInternal = [PdFloat (val0 + val1)]
      state' = setNodeState dst (PdNodeState inlets newInternal) state
   in
      forEachOutlet patch dst (sendMessage patch (PdSymbol "float" : newInternal)) state'

-- "osc~" object:

sendMessage patch [PdSymbol "float", PdFloat freq] (PdObject (PdSymbol "osc~" : _) _ _) (dst, 0) state@(PdState _ nss _) =
   let
      (PdNodeState inlets [_, position]) = index nss dst
   in
      setNodeState dst (PdNodeState inlets [PdFloat ((2 * pi) / (32000 / freq)), position]) state

-- "line~" object:

sendMessage patch [PdSymbol "float", PdFloat amp, PdFloat time] (PdObject [PdSymbol "line~"] _ _) (dst, 0) state@(PdState _ nss _) =
   let
      (PdNodeState inlets internal) = index nss dst
      [PdFloat current, PdFloat target, PdFloat delta] = if internal /= [] then internal else [PdFloat 0, PdFloat 0, PdFloat 0]
      newInternal = [PdFloat current, PdFloat amp, PdFloat ((amp - current) / (time * 32))]
   in
      setNodeState dst (PdNodeState inlets newInternal) state

-- cold inlets:

sendMessage patch (PdSymbol "float" : fs) node (dst, inl) state =
      (setInlet (dst, inl) fs state)

sendMessage patch atoms node (dst, inl) state =
      (setInlet (dst, inl) atoms state)

\end{code}

\subsection{Audio-handling objects}
\label{dsps}

\begin{code}

performDsp :: PdNode -> PdNodeState -> ([[PdAtom]], [PdAtom])

osc :: Double -> Double -> Double -> Double
osc position delta idx = (position + (delta * idx)) `mod'` (2 * pi)

performDsp obj@(PdObject [PdSymbol "osc~", PdFloat freq] _ _) ns@(PdNodeState inlets []) =
   performDsp obj (PdNodeState inlets [PdFloat ((2 * pi) / (32000 / freq)), PdFloat 0])

performDsp obj@(PdObject [PdSymbol "osc~", _] _ _) ns@(PdNodeState inlets [PdFloat delta, PdFloat position]) =
   let
      output = map (PdFloat . sin . osc position delta) [0..63]
      nextPosition = osc position delta 64
      newInternal = [PdFloat delta, PdFloat nextPosition]
   in
      ([output], newInternal)

performDsp obj@(PdObject [PdSymbol "line~"] _ _) ns@(PdNodeState inlets []) =
   performDsp obj (PdNodeState inlets [PdFloat 0, PdFloat 0, PdFloat 0])

performDsp obj@(PdObject [PdSymbol "line~"] _ _) ns@(PdNodeState inlets [PdFloat current, PdFloat target, PdFloat delta]) =
   let
      limiter = if delta > 0 then min else max
      output = map PdFloat $ tail $ take 65 $ iterate (\v -> limiter target (v + delta)) current
      newInternal = [last output, PdFloat target, PdFloat delta]
   in
      ([output], newInternal)

performDsp obj@(PdObject [PdSymbol "*~"] _ _) ns@(PdNodeState inlets []) =
   let
      mult (PdFloat a) (PdFloat b) = PdFloat (a * b)
      output = zipWith mult (index inlets 0) (index inlets 1)
   in
      ([output], [])

performDsp obj ns =
   ([toList $ replicate 64 $ PdFloat 0.0], [])

\end{code}

\section{Demonstration}

\begin{code}

cSharp  = 554.37
aSharp  = 932.33
g       = 783.99
gSharp  = 830.61
f       = 698.46

-- osc0.pd
patch = PdPatch 10 (fromList [
            PdAtomBox    [PdFloat 0], -- [PdControlInlet True "bang"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "osc~", PdFloat gSharp] 2 1, -- [PdControlInlet True "float", PdControlInlet True "float"] [PdSignalOutlet],
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdFloat 0.5), PdTAtom (PdFloat 1000)]], -- [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdFloat 0), PdTAtom (PdFloat 100)]], -- [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdObject     [PdSymbol "line~"] 2 1, -- [PdControlInlet True "list", PdControlInlet False "float"] [PdSignalOutlet],
            PdObject     [PdSymbol "*~"] 2 1, -- [PdSignalInlet 0, PdSignalInlet 0] [PdSignalOutlet],
            PdObject     [PdSymbol "dac~"] 1 0, -- [PdSignalInlet 0] [],
            PdObject     [PdSymbol "receive", PdSymbol "metroToggle"] 0 1, -- [] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "metro", PdFloat 500] 1 1, --  [PdControlInlet True "bang"] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "tabwrite~", PdSymbol "array99"] 1 0, -- [PdControlInlet True "signal"] [],
            PdObject    [PdSymbol "array99"] 0 0,
            PdMessageBox [PdCommand (PdReceiver "metroToggle") [PdTAtom (PdFloat 1.0)]], -- [] [],
            PdMessageBox [PdCommand (PdReceiver "metroToggle") [PdTAtom (PdFloat 0.0)]] -- [] []
         ]) (fromList [
            PdConnection (0, 0) (1, 0), -- 0 -> osc~
            PdConnection (1, 0) (5, 0), -- osc~ -> *~
            PdConnection (1, 0) (9, 0), -- osc~ -> tabwrite~
            PdConnection (7, 0) (8, 0), -- r -> metro
            PdConnection (8, 0) (9, 0), -- metro -> tabwrite~
            PdConnection (2, 0) (4, 0), -- 0.1 100 -> line~
            PdConnection (3, 0) (4, 0), -- 0 100 -> line~
            PdConnection (4, 0) (5, 1), -- line~ -> *~
            PdConnection (5, 0) (6, 0)  -- line~ -> dac~
         ]) [1, 4, 5, 9, 6]

convertData :: PdNodeState -> [Integer]
convertData ns@(PdNodeState inlets _) =
   let inlet = index inlets 0
   in map (\(PdFloat f) -> floor (((f + 1) / 2) * 65535)) inlet

everyOther :: [a] -> [a]
everyOther (x:(y:xs)) = x : everyOther xs
everyOther x = x

genOutput x = concat $ everyOther 
                     $ toList 
                     $ fmap (\state@(PdState _ nss _) -> convertData $ index nss 6) x
--genOutput x = x

main :: IO ()
main = print (genOutput $ runSteps 10000 patch [
                        (PdEvent 5 11 Nothing), -- metroToggle 1
                        (PdEvent 10 2 Nothing),  -- 0.1 1000
                        (PdEvent 900 3 Nothing), -- 0 100
                        (PdEvent 1001 0 (Just $ PdFloat cSharp)),
                        (PdEvent 1002 2 Nothing),  -- 0.1 1000

                        (PdEvent 1900 3 Nothing), -- 0 100
                        (PdEvent 2001 0 (Just $ PdFloat g)),
                        (PdEvent 2002 2 Nothing),  -- 0.1 1000

                        (PdEvent 3660 3 Nothing), -- 0 100
                        (PdEvent 3749 2 Nothing),  -- 0.1 1000
                        
                        (PdEvent 3750 0 (Just $ PdFloat gSharp)),
                        (PdEvent 3875 0 (Just $ PdFloat aSharp)),
                        (PdEvent 4000 0 (Just $ PdFloat gSharp)),

                        (PdEvent 4333 0 (Just $ PdFloat f)),

                        (PdEvent 4666 0 (Just $ PdFloat cSharp)),

                        (PdEvent 5000 0 (Just $ PdFloat g)),

                        (PdEvent 5650 3 Nothing), -- 0 100
                        (PdEvent 5745 2 Nothing),  -- 0.1 1000

                        (PdEvent 5750 0 (Just $ PdFloat gSharp)),
                        (PdEvent 5875 0 (Just $ PdFloat aSharp)),
                        (PdEvent 6000 0 (Just $ PdFloat gSharp)),

                        (PdEvent 7000 3 Nothing), -- 0 100

                        (PdEvent 4500 12 Nothing) -- metroToggle 0
             ])

\end{code}

\end{document}
