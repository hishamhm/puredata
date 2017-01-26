\documentclass[a4paper]{article}
\setlength{\parskip}{\baselineskip}
\usepackage[margin=3cm]{geometry}
%include lhs2TeX.fmt
\begin{document}

\title{An interpreter modelling the semantics of Pure Data}
\author{Hisham Muhammad}

\maketitle{}

This is an interpreter designed to model the core semantics of Pure Data, a
programmable music synthesis application, which features a dataflow language
at its core. The intention here is to describe the evaluation logic of the
language, so the goal here is clarity, not performance.

This implementation uses only standard modules included in the Haskell Platform:

\begin{code}
import Data.Sequence (Seq, fromList, index, update)
import qualified Data.Sequence (length, foldlWithIndex)
import Data.Foldable (foldl', toList)
import qualified Data.Foldable (foldl)
import Data.List (sort, intercalate)
import Text.Printf
import Data.Fixed
\end{code}

\section{Representation of programs}

A Pure Data program (called a ``patch'') is represented with the
\emph{PdPatch} data type in our model, which contains a sequence of nodes, a
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

data PdNode  =  PdObject      [PdAtom] Int Int
             |  PdAtomBox     PdAtom
             |  PdMessageBox  [PdCommand]
   deriving Show

\end{code}

Message boxes contain commands written in the textual sub-language of Pure
Data. Here, we represent commands not as a string, but in parsed form,
consisting of a receiver and a list of tokens (which may be literal values or
numbered references to inlet data (written in $\$n$ in the textual language).
Note that a single message box may contain a number of messages.

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
pairs, where each pair is represented as a \emph{PdConnection} value, itself
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
                   sTs     :: Int,
                   sNss    :: (Seq PdNodeState),
                   sLog    :: [String],
                   sSched  :: [PdEvent]
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
triggered, and an optional atom representing data entered by the user.

\begin{code}

data PdEvent =  PdEvent {
                   eTs    :: Int,
                   eNidx  :: Int,
                   eArg   :: (Maybe PdAtom)
                }
   deriving (Show, Eq, Ord)

\end{code}

%\begin{code}
%
%instance Show PdState where
%   show (PdState ts nss logw) =
%      "Step: "  ++ show ts
%                ++ " - Nodes: " 
%                ++ show (toList nss) 
%                ++ " - Log: {\n" 
%                ++ concatMap (\l -> l ++ "\n") logw 
%                ++ "}\n"
%
%instance Show PdNodeState where
%   show (PdNodeState ins atoms) =
%      "{" ++ show (toList ins) ++ " | " ++ (show atoms) ++ "}\n"
%
%\end{code}

\section{Execution}

The execution mode of Pure Data is data-driven. The user triggers events via
its interface, and those events cause a cascading series of firings. The user
may trigger events by clicking nodes, entering numbers (or using MIDI devices,
which are equivalent to entering numbers).

\subsection{Main loop}

The execution of the interpreter, therefore, is a loop of evaluation steps.
The main driver function takes a number of steps, the program to run, a list
of timed events, accumulating a list of states. We are interested in all
states, not only the final one, because we want to be able to inspect the
results of the execution over time.

Note that the program itself, \emph{p}, remains unchanged over time. This
is typical of a language with liveness level 2: the program cannot be modified
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
            s' = runStep p s currEvents

\end{code}

The loop above extracts the sublist of relevant events for the current
timestamp, and hands it over to the main evaluation function, \emph{runStep},
which, given a patch, the current state, and a list of events, produces
a new state.

The function \emph{runStep} processes events and the DSP tree. Following the
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
events (hence, \emph{runDspTree} is called at every other run of
\emph{runStep}). Assuming that a step in our interpreter is 1~ms, this means
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
may be entered into atom boxes.

Updating an atom box causes the new value to be fired to its outlet. We do it
producing a synthetic command (\emph{atomCmd}) when handling events on
\emph{PdAtomBox} objects. \emph{PdMessageBox} may contain multiple commands
(which are semicolon-separated in the textual syntax), so we fold over that
list, running each command.

\begin{code}

runEvent :: PdPatch -> PdState -> PdEvent -> PdState
runEvent p s event@(PdEvent ts nidx arg) =
   case (index (pNodes p) nidx) of
      PdAtomBox _ ->
         case arg of
            Just atom ->
               let atomCmd = PdCommand PdToOutlet [PdTAtom atom]
               in runCommand p nidx s atomCmd
            Nothing ->
               error "Events on atom boxes expect an argument."
      PdMessageBox cmds ->
         foldl' (runCommand p nidx) s cmds
      _ -> s

\end{code}

Pure Data commands are written in its textual language. Commands may include
references to data obtained via inlets of the node using the $\$n$ notation.
For example, sending \texttt{10 20} to a message box containing \texttt{pitch
\$2 velocity \$1} connected to an object box \texttt{print} will print to the
log window the string \texttt{pitch 20 velocity 10}.

In function \emph{runCommand} below, we run a given command \emph{cmd} on a
node (with index \emph{nidx}) by first obtaining the inlet data currently
stored in the node state. Then we perform $\$$-expansion on the command's
tokens. Then, based on the receiver of the message, we route it through the
graph (forwarding it to every outlet, in a classic dataflow fashion) or
symbolically, sending it to all objects configured as a receivers for the
given name.

\begin{code}

runCommand :: PdPatch -> Int -> PdState -> PdCommand -> PdState
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

dollarExpansion :: PdCommand -> [PdAtom] -> (PdReceiver, [PdAtom])
dollarExpansion (PdCommand recv tokens) inlData =
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

When sending a command, we propagate it to every connected outlet of a node.
This function takes the patch, a $\left(node, outlet\right)$ pair of indices
indicating the source of the data, the data itself (a list of atoms), and the
current state. It folds over the list of connections of the patch, firing
the data to the appropriate inlets of all matching connections.

\begin{code}

forEachInOutlet :: PdPatch -> (Int, Int) -> [PdAtom] -> PdState -> PdState
forEachInOutlet p srcPair atoms s =
   Data.Foldable.foldl handle s (pConns p)
   where
      handle :: PdState -> PdConnection -> PdState
      handle s (PdConnection from to@(dst, inl))
         | srcPair == from  = fire p (index (pNodes p) dst) atoms to s
         | otherwise        = s

\end{code}

The \emph{fire} function invokes the appropriate action for a node, producing
a new state.

\begin{code}

fire :: PdPatch -> PdNode -> [PdAtom] -> (Int, Int) -> PdState -> PdState

\end{code}

Depending on the type of node, we perform different actions. For message
boxes, we feed the incoming atoms into the inlet, and then we fold over its
triggering its commands, like when they are clicked by the user. As we
saw in \emph{runCommand}, this may fire further nodes either directly or
indirectly.

\begin{code}

fire p (PdMessageBox cmds) atoms (nidx, inl) s =
   let
      (PdNodeState ins mem) = index (sNss s) nidx
      ns' = PdNodeState (update inl atoms ins) mem
      s' = s { sNss = (update nidx ns' (sNss s)) }
   in
      foldl' (runCommand p nidx) s' cmds

\end{code}

For objects and atom boxes, we hand over the incoming data to the
\emph{sendMessage} handler function, which implements the various behaviors
supported by different Pure Data objects. The function \emph{sendMessage}
returns a tuple with the updated node state, log outputs produced (if any),
data to be sent via outlets and new events to be scheduled. We update the
state with this data. Finally, we propagate the data through the outlets,
processing them from right to left, as mandated by the Pure Data
specification.

\begin{code}

fire p node atoms (nidx, inl) s =
   let
      ns = index (sNss s) nidx
      (ns', logw', outlets, evs) = sendMessage node atoms inl ns
      s' = s {
         sNss    = update nidx ns' (sNss s),
         sLog    = (sLog s) ++ logw',
         sSched  = (sSched s) ++ evs
      }

      propagate :: PdState -> ([PdAtom], Int) -> PdState
      propagate s (atoms, outl) =
         if atoms == []
         then s
         else forEachInOutlet p (nidx, outl) atoms s
   in
      foldl' propagate s' (zip (reverse outlets) [length outlets - 1..0])

\end{code}

Indirect connections are handled similarly. Instead of folding over the list
of connections, we fold over the list of nodes, looking for objects declared
as \texttt{receive $name$}. Note that the search happens over the
statically-declared list of nodes of the patch. While it is possible construct
a message at runtime and determine the receiver dynamically, it is not
possible to change the identifier of a \texttt{receive} node at runtime.

\begin{code}

forEachReceiver :: PdPatch  -> String 
                            -> [PdAtom]
                            -> PdState -> PdState
forEachReceiver p name atoms s =
   Data.Sequence.foldlWithIndex handle s (pNodes p)
   where
      handle :: PdState -> Int -> PdNode -> PdState
      handle s dst (PdObject (PdSymbol "receive" : (PdSymbol rname : _)) _ _)
         | name == rname = forEachInOutlet p (dst, 0) atoms s
      handle s _ _ = s

\end{code}

\subsection{Audio processing}

The processing of audio nodes is very different from that of message nodes.
Before execution, the audio nodes are topologically sorted, producing an
order according to which they are evaluated on each DSP update. For simplicity,
we do not compute this order at the beginning of execution, and merely assume
it is given as an input (in the \emph{dspSort} field of \emph{p}).

As the list of nodes is traversed, each object is triggered (applying the
\emph{performDsp} function) and then the new computed value of its audio
buffer is propagated to the inlets of the nodes to which they are connected. 

\begin{code}

runDspTree :: PdPatch -> PdState -> PdState
runDspTree p s =
   s { sNss = nss' }
   where
      nss' = foldl handle (zeroDspInlets (sNss s) (pDspSort p)) (pDspSort p)

      handle :: (Seq PdNodeState) -> Int -> (Seq PdNodeState)
      handle nss nidx =
         let
            obj = index (pNodes p) nidx
            ns@(PdNodeState ins mem) = index nss nidx
            (outDsp, mem') = performDsp obj ns
            nss'' = update nidx (PdNodeState ins mem') nss
         in
            Data.Foldable.foldl (propagate outDsp) nss'' (pConns p)
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
   fromList $ clearNss 0 (toList nss) (sort dspSort)
      where
         zeroInlets :: Int -> (Seq [PdAtom])
         zeroInlets n = fromList $ replicate n (replicate 64 (PdFloat 0.0))

         zeroState :: PdNodeState -> PdNodeState
         zeroState (PdNodeState ins mem) =
            PdNodeState (zeroInlets (Data.Sequence.length ins)) mem

         clearNss :: Int -> [PdNodeState] -> [Int] -> [PdNodeState]
         clearNss i (st:sts) indices@(nidx:idxs)
            | i == nidx  = zeroState st  : clearNss (i+1) sts idxs
            | otherwise  = st            : clearNss (i+1) sts indices
         clearNss i  nss  []  = nss
         clearNss i  []   _   = []

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

In Section~\ref{dsps} we will present \emph{performDsp}, which implements the
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
         PdAtomBox     atom     -> PdNodeState (emptyInlets 1)    [atom]
         PdObject      _ inl _  -> PdNodeState (emptyInlets inl)  []
         PdMessageBox  _        -> PdNodeState (emptyInlets 1)    []

\end{code}

\section{Language constructs}

The graphical language of Pure Data is graph-based and contains only nodes and
edges. The contents of nodes (object boxes, message boxes and atom boxes) are
textual. Like there are two kinds of edges (message and audio), there are also
two kinds of objects. Audio-handling objects are identified by a \texttt{\~}
suffix in their names (the Pure Data documentation calls them ``tilde
objects''. In our interpreter, plain objects are implemented in the
\emph{sendMessage} function (Section~\ref{msgs}) and tilde objects are
implemented in the \emph{performDsp} function (Section~\ref{dsps}).

For printing to the log, we present a simple auxiliary function that adds to
the output log of the state value.

\begin{code}

printOut :: [PdAtom] -> PdState -> PdState
printOut atoms s =
   s { sLog = (sLog s) ++ [intercalate " " $ map show atoms] }

\end{code}

\subsection{Plain objects}
\label{msgs}

The implementation of all non-audio nodes is done in the \emph{sendMessage}
function, which pattern-matches on the structure of the node (which includes
the parsed representation of its textual definition). 

\begin{code}

sendMessage ::  PdNode -> [PdAtom] -> Int -> PdNodeState 
                -> (PdNodeState, [String], [[PdAtom]], [PdEvent])

\end{code}

Unlike the \emph{runCommand} function used in the firing of message boxes,
which causes global effects to the graph evaluation (via indirect connections)
and therefore needs access to the whole state, \emph{sendMessage} accesses
only the node's private state, producing a triple containing the new private
node state, any text produced for the output log, a list of messages to
be sent via the node's outlets and any new events to be scheduled.

We did not implement the full range of objects supported by Pure Data since
our goal was not to produce a full-fledged computer music application, but
we included a few representative objects that allow us to demonstrate the
interpreter and the various behaviors of objects.

\subsubsection{print}

The \texttt{print} object accepts data through its inlet and prints it to
the log console.

\begin{code}

sendMessage (PdObject (PdSymbol "print" : xs) _ _) (PdSymbol "float" : fs) 0 ns =
   (ns, ["print: " ++ (intercalate " " $ map show (xs ++ fs))], [], [])

sendMessage (PdObject (PdSymbol "print" : xs) _ _) (PdSymbol "list" : ls) 0 ns = 
   (ns, ["print: " ++ (intercalate " " $ map show (xs ++ ls))], [], [])

sendMessage (PdObject (PdSymbol "print" : xs) _ _) atoms 0 ns = 
   (ns, ["print: " ++ (intercalate " " $ map show atoms)], [], [])

\end{code}

\subsubsection{float}

In Pure Data, the first inlet of a node is the ``hot'' inlet; when data is
received through it, the action of the node is performed. When data arrives in
``cold'' inlets, it stays queued until the ``hot'' inlet causes the object to
be evaluated.

The \texttt{float} object has two inlets. When given a number through its
first inlet, it stores it in its internal memory and outputs the value through
the outlet. When given a number through its second inlet, it only stores the
value. When given a unit event (called \texttt{bang} in Pure Data), it outputs
the most currently received number (or the one given in its creation argument,
or zero as a fallback).

\begin{code}

sendMessage (PdObject [PdSymbol "float"] inl _) (PdSymbol "float" : fl) 0 _ =
   (PdNodeState (emptyInlets inl) fl, [], [PdSymbol "float" : fl], [])

sendMessage  (PdObject (PdSymbol "float" : xs) inl _) [PdSymbol "bang"] 0 
             (PdNodeState ins mem) =
   let
      inlet1 = index ins 1
      mem' = [head (inlet1 ++ mem ++ xs ++ [PdFloat 0])]
   in
      (PdNodeState (emptyInlets inl) mem', [], [PdSymbol "float" : mem'], [])

\end{code}

\subsubsection{+}

The \texttt{+} object also demonstrates the behavior of hot and cold inlets. 
When a number arrives in the hot inlet, it sums the values in inlets 0 and 1
and sends it through its outlet. When a \texttt{bang} arrives in the hot outlet,
the most recently received values in the inlet buffers are used for the sum instead.

\begin{code}

sendMessage  (PdObject [PdSymbol "+", n] _ _) [PdSymbol "float", fl] 0 
             (PdNodeState ins mem) =
   let
      (PdFloat val0) = fl
      inlet1 = index ins 1
      (PdFloat val1) = if inlet1 == [] then n else head inlet1
      mem' = [PdFloat (val0 + val1)]
      ns' = PdNodeState (update 0 [fl] ins) mem'
   in
      (ns', [], [PdSymbol "float" : mem'], [])

sendMessage  (PdObject [PdSymbol "+", n] _ _) [PdSymbol "bang"] 0 
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

\subsubsection{osc~}

Some audio objects in Pure Data also accept messages. The \texttt{osc~} object
implements a sinewave oscillator. Sending a float to it, we configure its
frequency, which is stored in the node's internal memory. Note that the actual
oscillator is not implemented here, but in the DSP handler for this object
type in function \emph{performDsp}, in Section~\ref{oscdsp}.

\begin{code}

sendMessage  (PdObject (PdSymbol "osc~" : _) _ _) [PdSymbol "float", PdFloat freq] 0 
             (PdNodeState ins [_, position]) =
   (PdNodeState ins [PdFloat ((2 * pi) / (32000 / freq)), position], [], [], [])

\end{code}

\subsubsection{line~}
\label{linemsg}

The \texttt{line~} object implements a linear function over time. It can be used,
for example, to implement gradual changes of frequency or amplitude. Its internal
memory stores values $current$, $target$ and $delta$. It accepts a message with
two floats, indicating the new target value and the time interval to take ramping
from the current value to the new target.

\begin{code}

sendMessage  (PdObject [PdSymbol "line~"] _ _)
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

\subsubsection{Atom boxes}

When given a float, atom boxes update their internal memory and propagate the
value. When given a \texttt{bang}, they just propagate the value.

\begin{code}

sendMessage (PdAtomBox _) (PdSymbol "float" : fl) 0 _ =
   (PdNodeState (fromList []) fl, [], [PdSymbol "float" : fl], [])

sendMessage (PdAtomBox _) [PdSymbol "bang"] 0 ns@(PdNodeState _ mem) =
   (ns, [], [PdSymbol "float" : mem], [])


\end{code}

\subsubsection{Cold inlets}

Since cold inlets are passive and only store the incoming data in the inlet
buffer without executing any node-specific operation, the implementation for
cold inlets can be shared by all types of node.

\begin{code}

sendMessage node (PdSymbol "float" : fs) inl (PdNodeState ins mem) =
   (PdNodeState (update inl fs ins) mem, [], [], [])

sendMessage node atoms inl (PdNodeState ins mem) =
   (PdNodeState (update inl atoms ins) mem, [], [], [])

\end{code}

\subsection{Audio-handling objects}
\label{dsps}

Similarly to \emph{sendMessage} in Section~\ref{msgs}, we define a single
function that performs the operations for all audio-processing objects:

\begin{code}

performDsp :: PdNode -> PdNodeState -> ([[PdAtom]], [PdAtom])

\end{code}

The \emph{performDsp} function takes the object, its node state and
outputs the audio buffer to be sent at the node's outlets, and the
updated internal data for the node.

We implement a small number of objects, that nevertheless will allow us to
make a realistic audio demonstration.

\subsubsection{osc~}
\label{oscdsp}

This is the sinewave oscillator. It holds two values in its internal memory,
\emph{delta} and \emph{position}, through which a wave describing a sine
function is incrementally computed.

We handle two cases here: when the internal memory is empty, the parameters
are initialized according to the \emph{freq} creation argument; when the 
memory is initialized, we produce the new buffer calculating
64 new values, determine the next position to start the wave in the next
iteration, store this value in the internal memory, and output the buffer
through the node's outlet.

\begin{code}

performDsp obj@(PdObject [PdSymbol "osc~", PdFloat freq] _ _) (PdNodeState ins []) =
   performDsp obj (PdNodeState ins [PdFloat ((2 * pi) / (32000 / freq)), PdFloat 0])

performDsp  (PdObject [PdSymbol "osc~", _] _ _)
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

\subsubsection{line~}

As described in Section~\ref{linemsg}, the \texttt{line~} object implements a
linear ramp over time. As in {osc~} we handle two cases: when the internal
memory of the object is empty, in which case we initialize it; and when it is
initialized with \emph{current}, \emph{target} and \emph{delta} values. The
function varies linearly over time from \emph{current} to \emph{target}, after
which, it stays constant at \emph{target}.

\begin{code}

performDsp obj@(PdObject [PdSymbol "line~"] _ _) (PdNodeState ins []) =
   performDsp obj (PdNodeState ins [PdFloat 0, PdFloat 0, PdFloat 0])

performDsp (PdObject [PdSymbol "line~"] _ _) (PdNodeState ins [PdFloat current, PdFloat target, PdFloat delta]) =
   let
      limiter = if delta > 0 then min else max
      output = map PdFloat $ tail $ take 65 $ iterate (\v -> limiter target (v + delta)) current
      mem' = [last output, PdFloat target, PdFloat delta]
   in
      ([output], mem')

\end{code}

\subsubsection{*~}

This object multiplies the data from inlets 0 and 1. It is used, for example,
to modify the amplitude of an audio wave.

\begin{code}

performDsp (PdObject [PdSymbol "*~"] _ _) (PdNodeState ins []) =
   let
      mult (PdFloat a) (PdFloat b) = PdFloat (a * b)
      output = zipWith mult (index ins 0) (index ins 1)
   in
      ([output], [])

\end{code}

\subsubsection{A default handler}

Finally, this is a default handler for \emph{performDsp} that merely produces
a silent audio buffer.

\begin{code}

performDsp obj ns =
   ([toList $ replicate 64 $ PdFloat 0.0], [])

\end{code}

\section{Demonstration}

To wrap up the presentation of the interpreter, we present a demonstration of
its use. We build a simple synthesizer with both frequency and amplitude
controllable via events, and use it to play a simple tune. First, we define
a few constants corresponding to the frequency in Hertz of some musical notes:

\begin{code}

cSharp  = 554.37
aSharp  = 932.33
g       = 783.99
gSharp  = 830.61
f       = 698.46

\end{code}

Then, we construct the patch:

\begin{code}

example = PdPatch (fromList [
            PdAtomBox     (PdFloat 0),
            PdObject      [PdSymbol "osc~", PdFloat gSharp] 2 1,
            PdMessageBox  [PdCommand PdToOutlet (map (PdTAtom . PdFloat) [0.5, 1000])],
            PdMessageBox  [PdCommand PdToOutlet (map (PdTAtom . PdFloat) [0, 100])],
            PdObject      [PdSymbol "line~"] 2 1,
            PdObject      [PdSymbol "*~"] 2 1,
            PdObject      [PdSymbol "dac~"] 1 0,
            PdObject      [PdSymbol "receive", PdSymbol "toggle"] 0 1,
            PdObject      [PdSymbol "metro", PdFloat 500] 1 1,
            PdObject      [PdSymbol "tabwrite~", PdSymbol "array99"] 1 0,
            PdObject      [PdSymbol "array99"] 0 0,
            PdMessageBox  [PdCommand (PdReceiver "toggle") [PdTAtom (PdFloat 1.0)]],
            PdMessageBox  [PdCommand (PdReceiver "toggle") [PdTAtom (PdFloat 0.0)]]
         ]) (fromList [
            PdConnection (0, 0) (1, 0),  -- 0 $\rightarrow$ osc~
            PdConnection (1, 0) (5, 0),  -- osc~ $\rightarrow$ *~
            PdConnection (1, 0) (9, 0),  -- osc~ $\rightarrow$ tabwrite~
            PdConnection (7, 0) (8, 0),  -- r $\rightarrow$ metro
            PdConnection (8, 0) (9, 0),  -- metro $\rightarrow$ tabwrite~
            PdConnection (2, 0) (4, 0),  -- 0.1 100 $\rightarrow$ line~
            PdConnection (3, 0) (4, 0),  -- 0 100 $\rightarrow$ line~
            PdConnection (4, 0) (5, 1),  -- line~ $\rightarrow$ *~
            PdConnection (5, 0) (6, 0)   -- line~ $\rightarrow$ dac~
         ]) [1, 4, 5, 9, 6]

\end{code}

This is the sequence of input events that corresponds to playing the tune:

\begin{code}

main :: IO ()
main =
   print 
   $ genOutput
   $ runSteps 10000 example [
      (PdEvent 5 11 Nothing), -- toggle 1
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
      
      (PdEvent 4500 12 Nothing) -- toggle 0
   ]

\end{code}

In Pure Data, the sound card is represented by the \texttt{dac~} object. Our
interpreter does nat handle actual audio output natively, but we can extract
the inlet data from that node from the list of states, and convert it to a
list of 16-bit integers, which is a format suitable for conversion into
audio formats.

\begin{code}

convertData :: PdNodeState -> [Integer]
convertData (PdNodeState ins _) =
   let inlet = index ins 0
   in map (\(PdFloat f) -> floor (((f + 1) / 2) * 65535)) inlet

everyOther :: [a] -> [a]
everyOther (x:(y:xs)) = x : everyOther xs
everyOther x = x

genOutput x = concat $ everyOther 
                     $ toList 
                     $ fmap (\(PdState _ nss _ _) -> convertData $ index nss 6) x

\end{code}

\begin{verbatim}
#!/bin/sh
./puredata | tr ',' '\n' | tr -d '[]' | lua -e '
for s in io.stdin:lines() do
   local n = tonumber(s)
   io.stdout:write(string.char(n // 256), string.char(n % 256))
end' > binario
rm output.wav
ffmpeg -f u16be -ar 32000 -ac 1 -i binario output.wav
mplayer output.wav

\end{verbatim}

%\begin{code}
%
%--
%-- messages.pd
%patch = PdPatch (fromList [
%            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 1), PdTAtom (PdFloat 2)], PdCommand PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 10), PdTAtom (PdFloat 20)]], 
%            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdSymbol "foo"), PdTAtom (PdFloat 5), PdTAtom (PdFloat 6)]], 
%            PdMessageBox [PdCommand PdToOutlet [PdTDollar 1, PdTDollar 1], PdCommand (PdRDollar 1) [PdTDollar 2], PdCommand (PdReceiver "bar") [PdTDollar 2, PdTDollar 1]], 
%            PdObject     [PdSymbol "print"] 1 0,
%            PdObject     [PdSymbol "print"] 1 0,
%            PdObject     [PdSymbol "r", PdSymbol "foo"] 0 1,
%            PdObject     [PdSymbol "print", PdSymbol "viaFoo"] 1 0,
%            PdObject     [PdSymbol "r", PdSymbol "bar"] 1 0,
%            PdObject     [PdSymbol "print", PdSymbol "viaBar"] 1 0
%         ]) (fromList [
%            PdConnection (0, 0) (2, 0), -- 1 2, 10 20 -> $1 $1...
%            PdConnection (1, 0) (2, 0), -- 4 5 6 -> $1 $1...
%            PdConnection (2, 0) (3, 0), -- $1 $1... -> print
%            PdConnection (2, 0) (4, 0), -- $1 $1... -> print
%            PdConnection (5, 0) (6, 0), -- r foo -> print viaFoo
%            PdConnection (7, 0) (8, 0)  -- r bar -> print viaBar
%         ]) []
%
%main :: IO ()
%main = print (runSteps 5 patch [(PdEvent 1 0 Nothing), (PdEvent 3 1 Nothing)])
%
%\end{code}

\end{document}
