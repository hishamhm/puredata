\documentclass[a4paper]{article}
\usepackage[margin=3cm]{geometry}

%BEGIN LYX PREAMBLE
\usepackage{dsfont}
\usepackage{amsmath}
\usepackage{graphicx}
%include polycode.fmt
%format nidx = "i_{N}"
%format val0 = "val_0"
%format val1 = "val_1"
%format inlet0 = "inlet_0"
%format inlet1 = "inlet_1"
%format currEvents = "events_{curr}"
%format nextEvents = "events_{next}"
%format oldAtoms = "atoms_{old}"
%format newAtoms = "atoms_{new}"
%format (PdConnection a b) = "(" a "\rhd " b ")"
%END LYX PREAMBLE

\begin{document}

\title{An interpreter modelling the semantics of Pure Data}
\author{Hisham Muhammad}

\maketitle{}

This is an interpreter designed to model the core semantics of Pure Data, a
programmable music synthesis application, which features a dataflow language
at its core. The intention here is to describe the evaluation logic of the
language, so the goal here is clarity, not performance.

%BEGIN LYX TEXT

This implementation uses only standard modules included in the Haskell Platform:

\begin{code}
import Data.Sequence (Seq, fromList, index, update, foldlWithIndex)
import qualified Data.Sequence as Seq (length)
import Data.Foldable (foldl', toList)
import Data.List (sort, intercalate, find)
import Text.Printf
import Data.Fixed
import Data.Binary.Put
import qualified Data.ByteString.Lazy as ByteString
import Control.Monad
import Debug.Trace
\end{code}

\section{Representation of programs}

A Pure Data program (called a ``patch'') is represented with the
|PdPatch| data type in our model, which contains a sequence of nodes, a
sequence of connections between nodes, and the pre-computed topological sort
of audio connections (stored as a list of integer indices).

\begin{code}
data PdPatch =  PdPatch {
                   pNodes    :: Seq PdNode,
                   pConns    :: Seq PdConnection,
                   pDspSort  :: [Int]
                }
\end{code}

The primitive values in Pure Data are called ``atoms'': they can be numbers,
strings (called ``symbols'') or opaque pointers. Opaque pointers are used by
graphical objects only, so those will be omitted here.

\begin{code}

data PdAtom  =  PdFloat     Double
             |  PdSymbol    String
   deriving (Eq, Ord)

instance Show PdAtom where
   show (PdFloat f)     = show f
   show (PdSymbol s)    = s

\end{code}

Nodes may be objects, atom boxes or message boxes. In Pure Data, objects are
initialized via ``creation arguments'': a string of arguments, represented
here as a list of atoms. We also store in an object its number of inlets and
outlets. Atom boxes and message boxes always have one inlet and one outlet
each.

\begin{code}

data PdNode  =  PdObj      [PdAtom] Int Int
             |  PdAtomBox  PdAtom
             |  PdMsgBox   [PdCmd]
   deriving Show

\end{code}

Message boxes contain commands written in the textual sub-language of Pure
Data. Here, we represent commands not as a string, but in parsed form,
consisting of a receiver and a list of tokens (which may be literal values or
numbered references to inlet data (written \texttt{\$\emph{n}} in the textual language).
Note that a single message box may contain a number of messages.

\begin{code}

data PdCmd = PdCmd PdReceiver [PdToken]
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
pairs, where each pair is represented as a |PdConnection| value, itself
composed of two pairs: the node index and outlet index for the source, and the
node index and inlet index for the destination. Throughout the interpreter, we
will often use the names $(src, outl)$ and $(dst, inl)$ to refer to those
indices.

\begin{code}

data PdConnection = PdConnection (Int, Int) (Int, Int)
   deriving Show

\end{code}

\section{Representation of states}

The representation of a state in our interpreter is a structure containing the
following values: the step count, which will double as our timestamp, since
Pure Data has time-based execution; the state for each node; the text contents
of the Pd logger window; and future events scheduled by the interpreter.

\begin{code}

data PdState =  PdState {
                   sTs       :: Int,
                   sNStates  :: (Seq PdNodeState),
                   sLog      :: [String],
                   sSched    :: [PdEvent]
                }
   deriving Show

\end{code}

The state for each node, on its turn, contains a sequence of atom buffers,
one for each inlet, and an internal memory (represented as a list of atoms).
Memory consumption during execution is therefore variable, characterizing
a dynamic dataflow model.

\begin{code}

data PdNodeState = PdNodeState (Seq [PdAtom]) [PdAtom]
   deriving Show

\end{code}

We represent events with a timestamp, the node index indicating which node was
triggered, and a list of atoms representing the event data (such as the number
entered by the user in an atom box).

\begin{code}

data PdEvent =  PdEvent {
                   eTs    :: Int,
                   eNidx  :: Int,
                   eArg   :: [PdAtom]
                }
   deriving (Show, Eq, Ord)

\end{code}

\section{Execution}

The execution mode of Pure Data is data-driven. The user triggers events via
its interface, and those events cause a cascading series of firings. The user
may trigger events by clicking nodes, entering numbers (or using MIDI devices,
which are equivalent to entering numbers).

\subsection{Main loop}

The execution of the interpreter, therefore, is a loop of evaluation steps.
The driver function takes a number of steps, the patch to run, a list of timed
events, accumulating a list of states. We are interested in all states, not
only the final one, because we want to be able to inspect the results of the
execution over time.

Note that the patch itself, |p|, remains unchanged over time. This
is typical of a language with liveness level 2: the patch cannot be modified
during execution.

\begin{code}

runSteps :: Int -> PdPatch -> [PdEvent] -> [PdState]
runSteps nSteps p events =
   reverse $ snd $ foldl' acc (events, [initialState p]) [0 .. (nSteps - 1)]
   where
      absTime :: [PdEvent] -> Int -> [PdEvent]
      absTime evs ts = map (\e -> e { eTs = (eTs e) + ts }) evs
      acc :: ([PdEvent], [PdState]) -> Int -> ([PdEvent], [PdState])
      acc (events, states@(s : ss)) step =
         (sort (nextEvents ++ absTime (sSched s') step), s' : states)
         where
            (currEvents, nextEvents) = span (\(PdEvent ts _ _) -> ts == step) events
            s' = runStep p (s { sSched = [] }) currEvents

\end{code}

The loop above extracts the sublist of relevant events for the current
timestamp, and hands it over to the main evaluation function, |runStep|,
which, given a patch, the current state, and a list of events, produces
a new state.

Processing a step may produce new future events to be scheduled. These are
sorted along with the existing events of the input. Runtime events are
produced by the interpreter using relative timestamps (where 0 means ``now''),
so we adjust them to absolute time using auxiliary function |adjTime|.

The function |runStep| processes events and the DSP tree. Following the
specified semantics of Pure Data, this happens in an alternating fashion: all
pending messages for a given timestamp are handled, and then the entire DSP
tree is processed.

\begin{code}

runStep :: PdPatch -> PdState -> [PdEvent] -> PdState
runStep p s events =
   let
      s' = runImmediateEvents p $ foldl' (runEvent p) s events
      s'' = if (sTs s) `mod` 2 == 0
            then runDspTree p s'
            else s'
   in
      s'' { sTs = (sTs s) + 1 }

\end{code}

In our model, the DSP tree is processed at half the rate of the message-based
events (hence, |runDspTree| is called at every other run of
|runStep|). Assuming that a step in our interpreter is 1~ms, this means
the DSP engine runs once every 2~ms (the default configuration of Pd runs the
engine every 1.45~ms; with a 64-sample buffer, this amounts to an audio sample
rate of 44,100~Hz — with this simplification in our interpreter, we get
36,000~Hz).

The Pure Data documentation specifies that "In the middle of a message cascade
you may schedule another one at a delay of zero. This delayed cascade happens
after the present cascade has finished, but at the same logical time". So,
events scheduled during the current step with a relative timestamp set to zero
are immediately executed before running the DSP tree:

\begin{code}

runImmediateEvents :: PdPatch -> PdState -> PdState
runImmediateEvents p s =
   let z = [ ev | ev <- (sSched s), eTs ev == 0 ]
   in if z == []
      then s
      else runStep p s z

\end{code}

\subsection{Event processing}

Two kinds of events can be triggered by the user. Message boxes may be
clicked, processing all commands stored inside them, or new numeric values
may be entered into atom boxes. We do it producing a synthetic firing
of the relevant node.

\begin{code}

runEvent :: PdPatch -> PdState -> PdEvent -> PdState
runEvent p s event@(PdEvent ts nidx args) =
   fire p (index (pNodes p) nidx) args (nidx, 0) s

\end{code}

The |fire| function invokes the appropriate action for a node, producing
a new state.

\begin{code}

fire :: PdPatch -> PdNode -> [PdAtom] -> (Int, Int) -> PdState -> PdState

\end{code}

Depending on the type of node, we perform different actions. For message
boxes, we feed the incoming atoms into the inlet, and then we fold over its
triggering its commands, like when they are clicked by the user. As we will
see below in the definition of |runCommand|, this may fire further nodes
either directly or indirectly.

\begin{code}

fire p (PdMsgBox cmds) atoms (nidx, inl) s =
   let
      (PdNodeState ins mem) = index (sNStates s) nidx
      ns' = PdNodeState (update inl atoms ins) mem
      s' = s { sNStates = (update nidx ns' (sNStates s)) }
   in
      foldl' (runCommand p nidx) s' cmds

\end{code}

For objects and atom boxes, we hand over the incoming data to the
|sendMsg| handler function, which implements the various behaviors
supported by different Pure Data objects. The function |sendMsg|
returns a tuple with the updated node state, log outputs produced (if any),
data to be sent via outlets and new events to be scheduled. We update the
state with this data, adjusting the node index of the returned events to point
them to that of the current node (|nidx|): a node can only schedule events for
itself. Finally, we propagate the data through the outlets, processing them
from right to left, as mandated by the Pure Data specification.

\begin{code}

fire p node atoms (nidx, inl) s =
   let
      ns = index (sNStates s) nidx
      (ns', logw', outlets, evs) = sendMsg node atoms inl ns
      s' = s {
         sNStates  = update nidx ns' (sNStates s),
         sLog      = (sLog s)    ++ logw',
         sSched    = (sSched s)  ++ (map (\e -> e { eNidx = nidx }) evs)
      }

      propagate :: PdState -> ([PdAtom], Int) -> PdState
      propagate s (atoms, outl) =
         if atoms == []
         then s
         else forEachInOutlet p (nidx, outl) atoms s
   in
      foldl' propagate s' (zip (reverse outlets) [length outlets - 1..0])

\end{code}

When propagating data, we send it to every connected outlet of a node. A node
may have multiple outlets and multiple nodes can be connected to a single
outlet. This function takes the patch, a $\left(node, outlet\right)$ pair of
indices indicating the source of the data, the data itself (a list of atoms),
and the current state. It folds over the list of connections of the patch,
firing the data to the appropriate inlets of all matching connections.

\begin{code}

forEachInOutlet :: PdPatch -> (Int, Int) -> [PdAtom] -> PdState -> PdState
forEachInOutlet p srcPair atoms s =
   foldl' handle s (pConns p)
   where
      handle :: PdState -> PdConnection -> PdState
      handle s (PdConnection from (to@(dst, inl)))
         | srcPair == from  = fire p (index (pNodes p) dst) atoms to s
         | otherwise        = s

\end{code}

Pure Data commands are written in its textual language. Commands may include
references to data obtained via inlets of the node using the $\$n$ notation.
For example, sending \texttt{10 20} to a message box containing \texttt{pitch
\$2 velocity \$1} connected to an object box \texttt{print} will print to the
log window the string \texttt{pitch 20 velocity 10}.

In function |runCommand| below, we run a given command |cmd| on a
node (with index |nidx|) by first obtaining the inlet data currently
stored in the node state. Then we perform $\$$-expansion on the command's
tokens. Then, based on the receiver of the message, we route it through the
graph (forwarding it to every outlet, in a classic dataflow fashion) or
symbolically, sending it to all objects configured as a receivers for the
given name.

\begin{code}

runCommand :: PdPatch -> Int -> PdState -> PdCmd -> PdState
runCommand p nidx (PdState ts nss logw evs) cmd =
   let
      (PdNodeState ins mem) = index nss nidx
      inletData = index ins 0
      (recv, atoms) = dollarExpansion cmd inletData
      ns' = PdNodeState (update 0 [] ins) mem
      nss' = update nidx ns' nss
      s' = PdState ts nss' logw evs
   in
      case recv of
      PdToOutlet ->
         forEachInOutlet p (nidx, 0) atoms s'
      PdReceiver r ->
         forEachReceiver p r atoms s'
      PdReceiverErr ->
         printOut [PdSymbol "$1: symbol needed as message destination"] s'

\end{code}

The process of $\$$-expansion is a simple substitution, where the receiver
must be a string. Invalid indices are converted to zero. (In Pure Data, they
also produce an error message to the log window, but here we omit this for
brevity.) We also handle here a few syntactic shortcuts: a messages with a
sole number like \texttt{1.0} expands to \texttt{float 1.0}; lists starting
with a number get the prefix \texttt{list}.

\begin{code}

ffor a f = fmap f a

dollarExpansion :: PdCmd -> [PdAtom] -> (PdReceiver, [PdAtom])
dollarExpansion (PdCmd recv tokens) inlData =
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
      normalize atoms@[PdFloat f]       = (PdSymbol "float" : atoms)
      normalize atoms@(PdFloat f : xs)  = (PdSymbol "list" : atoms)
      normalize atoms                   = atoms

\end{code}

Indirect connections are handled similarly to outlet connections, but instead
of folding over the list of connections, we fold over the list of nodes,
looking for objects declared as \texttt{receive $name$}. Note that the search
happens over the statically-declared list of nodes of the patch. While it is
possible construct a message at runtime and determine the receiver
dynamically, it is not possible to change the identifier of a \texttt{receive}
node at runtime.

\begin{code}

forEachReceiver :: PdPatch  -> String 
                            -> [PdAtom]
                            -> PdState -> PdState
forEachReceiver p name atoms s =
   foldlWithIndex handle s (pNodes p)
   where
      handle :: PdState -> Int -> PdNode -> PdState
      handle s dst (PdObj (PdSymbol "receive" : (PdSymbol rname : _)) _ _)
         | name == rname = forEachInOutlet p (dst, 0) atoms s
      handle s _ _ = s

\end{code}

\subsection{Audio processing}

The processing of audio nodes is very different from that of message nodes.
Before execution, the audio nodes are topologically sorted, producing an
order according to which they are evaluated on each DSP update. For simplicity,
we do not compute this order at the beginning of execution, and merely assume
it is given as an input (in the |dspSort| field of |p|).

As the list of nodes is traversed, each object is triggered (applying the
|performDsp| function) and then the new computed value of its audio
buffer is propagated to the inlets of the nodes to which they are connected. 

\begin{code}

runDspTree :: PdPatch -> PdState -> PdState
runDspTree p s =
   s { sNStates = nss' }
   where
      nss' = foldl' handle (zeroDspInlets (sNStates s) (pDspSort p)) (pDspSort p)

      handle :: (Seq PdNodeState) -> Int -> (Seq PdNodeState)
      handle nss nidx =
         foldl' (propagate outputs) nss'' (pConns p)
         where
            obj = index (pNodes p) nidx
            ns@(PdNodeState ins mem) = index nss nidx
            (outputs, mem') = performDsp obj ns
            nss'' = update nidx (PdNodeState ins mem') nss
            propagate :: [[PdAtom]] -> (Seq PdNodeState) -> PdConnection -> (Seq PdNodeState)
            propagate outputs nss (PdConnection (src, outl) (dst, inl))
               | src == nidx  = addToInlet (dst, inl) (outputs !! outl) nss
               | otherwise    = nss

\end{code}

Each audio node has a 64-sample buffer that needs to be cleared before each
traversal. Note that this is different from handling inlets in message
objects: for message objects, the inlets become empty once consumed. Here, we
need the inlet buffers to be filled with zeros.

\begin{code}

zeroDspInlets :: (Seq PdNodeState) -> [Int] -> (Seq PdNodeState)
zeroDspInlets nss dspSort =
   fromList $ clearNodes 0 (toList nss) (sort dspSort)
      where
         zeroInlets :: Int -> (Seq [PdAtom])
         zeroInlets n = fromList $ replicate n (replicate 64 (PdFloat 0.0))

         zeroState :: PdNodeState -> PdNodeState
         zeroState (PdNodeState ins mem) =
            PdNodeState (zeroInlets (Seq.length ins)) mem

         clearNodes :: Int -> [PdNodeState] -> [Int] -> [PdNodeState]
         clearNodes nidx (st:sts) indices@(i:is)
            | nidx == i  = zeroState st  : clearNodes (nidx+1) sts is
            | otherwise  = st            : clearNodes (nidx+1) sts indices
         clearNodes nidx  nss  []  = nss
         clearNodes nidx  []   _   = []

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
      ns@(PdNodeState ins mem) = index nss dst
      oldAtoms = index ins inl
      newAtoms = fmap satSum (zip oldAtoms atoms)
      ns' = PdNodeState (update inl newAtoms ins) mem

\end{code}

In Section~\ref{dsps} we will present |performDsp|, which implements the
various DSP objects supported by this interpreter.

\subsection{Initial state}

Finally, for completeness of the execution model, we present here the
functions that create the initial state.

\begin{code}

emptyInlets :: Int -> Seq [PdAtom]
emptyInlets n = fromList (replicate n [])

initialState :: PdPatch -> PdState
initialState (PdPatch nodes _ _) = PdState 0 (fmap emptyNode nodes) [] []
   where 
   emptyNode node =
      case node of
         PdAtomBox  atom     -> PdNodeState (emptyInlets 1)    [atom]
         PdObj      _ inl _  -> PdNodeState (emptyInlets inl)  []
         PdMsgBox   _        -> PdNodeState (emptyInlets 1)    []

\end{code}

\section{Operations}

The graphical language of Pure Data is graph-based and contains only nodes and
edges. The contents of nodes (object boxes, message boxes and atom boxes) are
textual. Like there are two kinds of edges (message and audio), there are also
two kinds of objects. Audio-handling objects are identified by a \texttt{\~}
suffix in their names (the Pure Data documentation calls them ``tilde
objects''. In our interpreter, plain objects are implemented in the
|sendMsg| function (Section~\ref{msgs}) and tilde objects are
implemented in the |performDsp| function (Section~\ref{dsps}).

For printing to the log, we present a simple auxiliary function that adds to
the output log of the state value.

\begin{code}

printOut :: [PdAtom] -> PdState -> PdState
printOut atoms s =
   s { sLog = (sLog s) ++ [intercalate " " $ map show atoms] }

\end{code}

The implementation of all non-audio nodes is done in the |sendMsg|
function, which pattern-matches on the structure of the node (which includes
the parsed representation of its textual definition). 

\begin{code}

sendMsg ::  PdNode -> [PdAtom] -> Int -> PdNodeState 
            -> (PdNodeState, [String], [[PdAtom]], [PdEvent])

\end{code}

Unlike the |runCommand| function used in the firing of message boxes,
which causes global effects to the graph evaluation (via indirect connections)
and therefore needs access to the whole state, |sendMsg| accesses
only the node's private state, producing a triple containing the new private
node state, any text produced for the output log, a list of messages to
be sent via the node's outlets and any new events to be scheduled.

Similarly to |sendMsg|, we define a single function that performs the
operations for all audio-processing objects:

\begin{code}

performDsp :: PdNode -> PdNodeState -> ([[PdAtom]], [PdAtom])

\end{code}

The |performDsp| function takes the object, its node state and
outputs the audio buffer to be sent at the node's outlets, and the
updated internal data for the node.

We did not implement the full range of objects supported by Pure Data since
our goal was not to produce a full-fledged computer music application, but
we included a few representative objects that allow us to demonstrate the
interpreter and the various behaviors of objects.

\subsection{Atom boxes}

When given a float, atom boxes update their internal memory and propagate the
value. When given a \texttt{bang}, they just propagate the value.

\begin{code}

sendMsg (PdAtomBox _) (PdSymbol "float" : fl) 0 _ =
   (PdNodeState (fromList []) fl, [], [PdSymbol "float" : fl], [])

sendMsg (PdAtomBox _) [PdSymbol "bang"] 0 ns@(PdNodeState _ mem) =
   (ns, [], [PdSymbol "float" : mem], [])


\end{code}

\subsection{An object with side-effects: \texttt{print}}

The \texttt{print} object accepts data through its inlet and prints it to
the log console. It demonstrates the use of the log console as a global
side-effect.

\begin{code}

sendMsg (PdObj (PdSymbol "print" : xs) _ _) (PdSymbol "float" : fs) 0 ns =
   (ns, ["print: " ++ (intercalate " " $ map show (xs ++ fs))], [], [])

sendMsg (PdObj (PdSymbol "print" : xs) _ _) (PdSymbol "list" : ls) 0 ns = 
   (ns, ["print: " ++ (intercalate " " $ map show (xs ++ ls))], [], [])

sendMsg (PdObj (PdSymbol "print" : xs) _ _) atoms 0 ns = 
   (ns, ["print: " ++ (intercalate " " $ map show atoms)], [], [])

\end{code}

\subsection{An object with hot and cold inlets: \texttt{+}}

In Pure Data, the first inlet of a node is the ``hot'' inlet; when data is
received through it, the action of the node is performed. When data arrives in
``cold'' inlets, it stays queued until the ``hot'' inlet causes the object to
be evaluated.

The \texttt{+} object demonstrates the behavior of hot and cold inlets. 
When a number arrives in the hot inlet, it sums the values in inlets 0 and 1
and sends it through its outlet. When a \texttt{bang} arrives in the hot outlet,
the most recently received values in the inlet buffers are used for the sum instead.

\begin{code}

sendMsg  (PdObj [PdSymbol "+", n] _ _) [PdSymbol "float", fl] 0 
             (PdNodeState ins mem) =
   let
      (PdFloat val0) = fl
      inlet1 = index ins 1
      (PdFloat val1) = if inlet1 == [] then n else head inlet1
      mem' = [PdFloat (val0 + val1)]
      ns' = PdNodeState (update 0 [fl] ins) mem'
   in
      (ns', [], [PdSymbol "float" : mem'], [])

sendMsg  (PdObj [PdSymbol "+", n] _ _) [PdSymbol "bang"] 0 
             (PdNodeState ins mem) =
   let
      inlet0 = index ins 0
      (PdFloat val0) = if inlet0 == [] then (PdFloat 0) else head inlet0
      inlet1 = index ins 1
      (PdFloat val1) = if inlet1 == [] then n else head inlet1
      mem' = [PdFloat (val0 + val1)]
      ns' = PdNodeState ins mem'
   in
      (ns', [], [PdSymbol "float" : mem'], [])

\end{code}

\subsection{Objects producing timed events: @delay@ and @metro@}

The \texttt{delay} object demonstrates how objects generate future events.
We handle four cases: receiving a \texttt{bang} message schedules a
\texttt{tick} event. When received, it outputs a \texttt{bang} to the 
node's outlets.

\begin{code}

sendMsg (PdObj [PdSymbol "delay", PdFloat time] inl _) [PdSymbol "bang"] 0 ns =
   (ns, [], [], [PdEvent (floor time) 0 [PdSymbol "tick"]])

sendMsg (PdObj (PdSymbol "delay" : t) inl _) [PdSymbol "tick"] 0 ns =
   (ns, [], [[PdSymbol "bang"]], [])

\end{code}

The \texttt{metro} node, on its turn, expands on the \texttt{delay}
functionality, implementing a metronome: it sends a series of \texttt{bang}
messages at regular time intervals. It also has a second inlet which allows
updating the interval.

We handle four cases: receiving a \texttt{bang} message to start the
metronome, receiving a \texttt{stop} message to stop it, and receiving the
internally-scheduled \texttt{tick} when the metronome is either on or off.

\begin{code}

sendMsg  (PdObj (PdSymbol "metro" : xs) inl _) [PdSymbol "bang"] 0 
             (PdNodeState ins mem) =
   let
      inlet1 = index ins 1
      (PdFloat time) = head (inlet1 ++ mem ++ xs ++ [PdFloat 1000])
      ns' = PdNodeState (emptyInlets inl) [PdFloat time, PdSymbol "on"]
   in
      (ns', [], [[PdSymbol "bang"]], [PdEvent (floor time) 0 [PdSymbol "tick"]])

sendMsg  (PdObj (PdSymbol "metro" : xs) inl _) [PdSymbol "stop"] 0 
             (PdNodeState ins [PdFloat time, PdSymbol "on"]) =
   (PdNodeState ins [PdFloat time, PdSymbol "off"], [], [], [])

sendMsg  (PdObj (PdSymbol "metro" : xs) inl _) [PdSymbol "tick"] 0 
             ns@(PdNodeState ins [PdFloat time, PdSymbol "on"]) =
   (ns, [], [[PdSymbol "bang"]], [PdEvent (floor time) 0 [PdSymbol "tick"]])

sendMsg  (PdObj (PdSymbol "metro" : xs) inl _) [PdSymbol "tick"] 0 
             ns@(PdNodeState ins [_, PdSymbol "off"]) =
   (ns, [], [], [])

\end{code}

\subsection{Message handlers for audio objects: @osc~@ and @line~@}
\label{linemsg}

Some audio objects in Pure Data also accept messages. The \texttt{osc\~} object
implements a sinewave oscillator. Sending a float to it, we configure its
frequency, which is stored in the node's internal memory. Note that the actual
oscillator is not implemented here, but in the DSP handler for this object
type in function |performDsp|, in Section~\ref{oscdsp}.

\begin{code}

sendMsg  (PdObj (PdSymbol "osc~" : _) _ _) [PdSymbol "float", PdFloat freq] 0 
             (PdNodeState ins [_, position]) =
   (PdNodeState ins [PdFloat ((2 * pi) / (32000 / freq)), position], [], [], [])

\end{code}

The \texttt{line\~} object implements a linear function over time. It can be used,
for example, to implement gradual changes of frequency or amplitude. Its internal
memory stores values $current$, $target$ and $delta$. It accepts a message with
two floats, indicating the new target value and the time interval to take ramping
from the current value to the new target.

\begin{code}

sendMsg  (PdObj [PdSymbol "line~"] _ _)
             [PdSymbol "list", PdFloat amp, PdFloat time] 0 
             (PdNodeState ins mem) =
   let
      [PdFloat current, PdFloat target, PdFloat delta] =
         if mem /= [] then mem else [PdFloat 0, PdFloat 0, PdFloat 0]
      mem' =
         [PdFloat current, PdFloat amp, PdFloat ((amp - current) / (time * 32))]
   in
      (PdNodeState ins mem', [], [], [])

\end{code}

\subsection{Cold inlets}

Since cold inlets are passive and only store the incoming data in the inlet
buffer without executing any node-specific operation, the implementation for
cold inlets can be shared by all types of node.

\begin{code}

sendMsg node (PdSymbol "float" : fs) inl (PdNodeState ins mem) | inl > 0 =
   (PdNodeState (update inl fs ins) mem, [], [], [])

sendMsg node atoms inl (PdNodeState ins mem) | inl > 0 =
   (PdNodeState (update inl atoms ins) mem, [], [], [])

\end{code}

\subsection{Data objects: \texttt{float} and \texttt{list}}

The \texttt{float} and \texttt{list} objects store and forward data of their
respective types. They have two inlets for accepting new data. When given data
through its first inlet, the object stores it in its internal memory and
outputs the value through the outlet. When given data through its second
inlet, it only stores the value. When given a unit event (called \texttt{bang}
in Pure Data), it outputs the most recently received value (or the one given
in its creation argument, or zero as a fallback).

\begin{code}

sendMsg cmd@(PdObj (PdSymbol "float" : xs) inl _) atoms 0 ns =
   dataObject cmd atoms ns

sendMsg cmd@(PdObj (PdSymbol "list" : xs) inl _) atoms 0 ns =
   dataObject cmd atoms ns

dataObject (PdObj (PdSymbol a : xs) inl _) [PdSymbol "bang"] 
           (PdNodeState ins mem) =
   let
      inlet1 = index ins 1
      Just mem' = find (/= []) [inlet1, mem, xs, [PdFloat 0]]
   in
      (PdNodeState (emptyInlets inl) mem', [], [PdSymbol a : mem'], [])

dataObject (PdObj (PdSymbol a : xs) inl _) (PdSymbol b : fl) _ | a == b =
   (PdNodeState (emptyInlets inl) fl, [], [PdSymbol a : fl], [])

\end{code}

\subsection{Audio handling operations: @osc~@, @line~@ and @*~@}
\label{dsps}
\label{oscdsp}

Audio handling is performed by function |performDsp|, which implements
cases for each type of audio object.

Object \texttt{osc\~} is the sinewave oscillator. It holds two values in its
internal memory, |delta| and |position|, through which a wave describing a
sine function is incrementally computed.

We handle two cases here: when the internal memory is empty, the parameters
are initialized according to the |freq| creation argument; when the 
memory is initialized, we produce the new buffer calculating
64 new values, determine the next position to start the wave in the next
iteration, store this value in the internal memory, and output the buffer
through the node's outlet.

\begin{code}

performDsp obj@(PdObj [PdSymbol "osc~", PdFloat freq] _ _) (PdNodeState ins []) =
   performDsp obj (PdNodeState ins [PdFloat ((2 * pi) / (32000 / freq)), PdFloat 0])

performDsp  (PdObj [PdSymbol "osc~", _] _ _)
            (PdNodeState ins [PdFloat delta, PdFloat position]) =
   let
      osc :: Double -> Double -> Double -> Double
      osc position delta idx = (position + (delta * idx)) `mod'` (2 * pi)

      output = map (PdFloat . sin . osc position delta) [0..63]
      nextPosition = osc position delta 64
      mem' = [PdFloat delta, PdFloat nextPosition]
   in
      ([output], mem')

\end{code}

As described in Section~\ref{linemsg}, the \texttt{line\~} object implements a
linear ramp over time. As in \texttt{osc\~} we handle two cases: when the internal
memory of the object is empty, in which case we initialize it; and when it is
initialized with |current|, |target| and |delta| values. The
function varies linearly over time from |current| to |target|, after
which, it stays constant at |target|.

\begin{code}

performDsp obj@(PdObj [PdSymbol "line~"] _ _) (PdNodeState ins []) =
   performDsp obj (PdNodeState ins [PdFloat 0, PdFloat 0, PdFloat 0])

performDsp  (PdObj [PdSymbol "line~"] _ _)
            (PdNodeState ins [PdFloat current, PdFloat target, PdFloat delta]) =
   let
      limiter = if delta > 0 then min else max
      output =  map PdFloat $ tail $ take 65
                $ iterate (\v -> limiter target (v + delta)) current
      mem' = [last output, PdFloat target, PdFloat delta]
   in
      ([output], mem')

\end{code}

The @*~@ object multiplies the data from inlets 0 and 1. It is used, for example,
to modify the amplitude of an audio wave.

\begin{code}

performDsp (PdObj [PdSymbol "*~"] _ _) (PdNodeState ins []) =
   let
      mult (PdFloat a) (PdFloat b) = PdFloat (a * b)
      output = zipWith mult (index ins 0) (index ins 1)
   in
      ([output], [])

\end{code}

Finally, this is a default handler for |performDsp| that merely produces
a silent audio buffer.

\begin{code}

performDsp obj ns =
   ([toList $ replicate 64 $ PdFloat 0.0], [])

\end{code}

%END LYX TEXT

\section{Demonstration}

%BEGIN LYX DEMO

To wrap up the presentation of the interpreter modeling Pure Data, we present
a demonstration of its use. We build a simple synthesizer with both frequency
and amplitude controllable via events, and use it to play the motif from the
main theme of the film ``Back To The Future'', composed by Alan Silvestri.

First, we define a few constants corresponding to the frequency in Hertz of
some musical notes:

\begin{code}

cSharp  = 554.37
aSharp  = 932.33
g       = 783.99
gSharp  = 830.61
f       = 698.46

\end{code}

Then, we construct the patch that corresponds to the following graph:

\begin{center}
\includegraphics[width=0.75\columnwidth]{puredata_example}
\par\end{center}

\begin{code}

example = PdPatch (fromList [
   PdAtomBox  (PdFloat 0), -- 0
   PdObj      [PdSymbol "osc~", PdFloat gSharp] 2 1, -- 1
   PdMsgBox   [PdCmd PdToOutlet (map (PdTAtom . PdFloat) [0.5, 1000])], -- 2
   PdMsgBox   [PdCmd PdToOutlet (map (PdTAtom . PdFloat) [0, 100])], -- 3
   PdObj      [PdSymbol "line~"] 2 1, -- 4
   PdObj      [PdSymbol "*~"] 2 1, -- 5
   PdObj      [PdSymbol "dac~"] 1 0, -- 6

   PdObj      [PdSymbol "receive", PdSymbol "MyMetro"] 0 1, -- 7
   PdObj      [PdSymbol "metro", PdFloat 500] 2 1, -- 8
   PdObj      [PdSymbol "delay", PdFloat 5] 2 1, -- 9
   PdObj      [PdSymbol "list", PdFloat 0.5, PdFloat 0.1] 2 1, -- 10
   PdObj      [PdSymbol "list", PdFloat 0, PdFloat 500] 2 1, -- 11
   PdObj      [PdSymbol "line~"] 1 1, -- 12
   PdObj      [PdSymbol "osc~", PdFloat (gSharp / 2)] 1 1, -- 13
   PdObj      [PdSymbol "*~"] 2 1, -- 14

   PdMsgBox   [PdCmd PdToOutlet
                    [PdTAtom (PdSymbol "list"), PdTAtom (PdSymbol "bang")]], -- 15
   PdMsgBox   [PdCmd PdToOutlet
                    [PdTAtom (PdSymbol "list"), PdTAtom (PdSymbol "stop")]], -- 16
   PdMsgBox   [PdCmd (PdReceiver "MyMetro") [PdTDollar 1]]] -- 17
   
   ) (fromList [
      PdConnection (0,   0) (1,   0),  PdConnection (1,   0) (5,   0),  PdConnection (2,   0) (4,   0),
      PdConnection (3,   0) (4,   0),  PdConnection (4,   0) (5,   1),  PdConnection (5,   0) (6,   0),
      PdConnection (7,   0) (8,   0),  PdConnection (8,   0) (9,   0),  PdConnection (8,   0) (10,  0),
      PdConnection (9,   0) (11,  0),  PdConnection (10,  0) (12,  0),  PdConnection (11,  0) (12,  0),
      PdConnection (12,  0) (14,  0),  PdConnection (13,  0) (14,  1),  PdConnection (14,  0) (6,   0),
      PdConnection (15,  0) (17,  0),  PdConnection (16,  0) (17,  0)]
   ) [1, 4, 5, 12, 13, 14, 6]

\end{code}

This is the sequence of input events that corresponds to playing the tune:

\begin{code}

main :: IO ()
main =
   ByteString.putStr $ runPut (putWav output)
   where
   output = genOutput $ runSteps 10000 example [
      (PdEvent 1000 15 [PdSymbol "bang"]), -- MyMetro bang
      (PdEvent 1010 2 [PdSymbol "bang"]),  -- 0.1 1000
      (PdEvent 1900 3 [PdSymbol "bang"]), -- 0 100
      (PdEvent 2001 0 [PdSymbol "float", PdFloat cSharp]),
      (PdEvent 2002 2 [PdSymbol "bang"]),  -- 0.1 1000
      
      (PdEvent 2900 3 [PdSymbol "bang"]), -- 0 100
      (PdEvent 3001 0 [PdSymbol "float", PdFloat g]),
      (PdEvent 3002 2 [PdSymbol "bang"]),  -- 0.1 1000
      
      (PdEvent 4660 3 [PdSymbol "bang"]), -- 0 100
      (PdEvent 4749 2 [PdSymbol "bang"]),  -- 0.1 1000
      
      (PdEvent 4750 0 [PdSymbol "float", PdFloat gSharp]),
      (PdEvent 4875 0 [PdSymbol "float", PdFloat aSharp]),
      (PdEvent 5000 0 [PdSymbol "float", PdFloat gSharp]),
      
      (PdEvent 5333 0 [PdSymbol "float", PdFloat f]),
      
      (PdEvent 5666 0 [PdSymbol "float", PdFloat cSharp]),
      
      (PdEvent 6000 0 [PdSymbol "float", PdFloat g]),
      
      (PdEvent 6650 3 [PdSymbol "bang"]), -- 0 100
      (PdEvent 6745 2 [PdSymbol "bang"]),  -- 0.1 1000
      
      (PdEvent 6750 0 [PdSymbol "float", PdFloat gSharp]),
      (PdEvent 6875 0 [PdSymbol "float", PdFloat aSharp]),
      (PdEvent 7000 0 [PdSymbol "float", PdFloat gSharp]),

      (PdEvent 7000 16 [PdSymbol "bang"]), -- MyMetro stop
      
      (PdEvent 8000 3 [PdSymbol "bang"])] -- 0 100

\end{code}

In Pure Data, the sound card is represented by the \texttt{dac\~} object. Our
interpreter does nat handle actual audio output natively, but we can extract
the inlet data from that node from the list of states, and convert it to an
audio \texttt{wav} file format, that is then sent to standard output.

\begin{code}

convertData :: PdNodeState -> [Integer]
convertData (PdNodeState ins _) =
   let inlet = index ins 0
   in map (\(PdFloat f) -> floor (f * 32768)) inlet

everyOther :: [a] -> [a]
everyOther (x:(y:xs)) = x : everyOther xs
everyOther x = x

genOutput x = concat $ everyOther 
                     $ toList 
                     $ fmap (\(PdState _ nss _ _) -> convertData $ index nss 6) x

putWav vs =
   let
      riff  = 0x46464952
      wave  = 0x45564157
      fmts  = 0x20746d66
      datx  = 0x61746164
      formatHeaderLen = 16
      fileSize = (44 + (length vs) * 2)
      bitsPerSample = 16
      format = 1
      channels = 1
      sampleRate = 32000
   in do
      putWord32le riff
      putWord32le (fromIntegral fileSize)
      putWord32le wave
      putWord32le fmts
      putWord32le formatHeaderLen
      putWord16le format
      putWord16le channels
      putWord32le sampleRate
      putWord32le (sampleRate * bitsPerSample * (fromIntegral channels) `div` 8)
      putWord16le (((fromIntegral bitsPerSample) * channels) `div` 8)
      putWord16le (fromIntegral bitsPerSample)
      putWord32le datx
      putWord32le (fromIntegral ((length vs) * 2))
      mapM_ (putWord16le . fromIntegral) vs

\end{code}

%END LYX DEMO

\end{document}
