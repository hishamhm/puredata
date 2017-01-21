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
import Data.Foldable (toList)
import qualified Data.Foldable (foldl)
import Debug.Trace
import Data.List (sort)
import Text.Printf
import Data.Fixed
\end{code}

\section{Representation of programs}

A Pure Data program is represented with the {\tt PdPatch} data type in our model,
which contains the DSP buffer size, a sequence of nodes, a sequence of
connections between nodes, and the pre-computed topological sort of audio
connections (stored as a list of integer indices).

\begin{code}
data PdPatch = PdPatch PdBufferSize (Seq PdNode) (Seq PdConnection) [Int]

type PdBufferSize = Int
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
initialized via "creation arguments": a string of arguments, represented here
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
numbered references to inlets (written in {\tt \$}$n$ in the textual
language).

\begin{code}

data PdCommand = PdCommand PdReceiver [PdToken]
   deriving Show

data PdReceiver  =  PdToOutlet
                 |  PdReceiver String
                 |  PdRDollar Int
                 |  PdReceiverErr
   deriving Show

data PdToken = PdTDollar  Int
             | PdTAtom    PdAtom
   deriving Show

\end{code}

Finally, we represent the connections of the graph as a sequence of adjacency
pairs, where each pair is represented as a {\tt PdConnection}, itself composed
of two pairs: the node index and outlet index for the source, and the node
index and inlet index for the destination. 

\begin{code}

data PdConnection = PdConnection (Int, Int) (Int, Int)
   deriving Show

\end{code}

\section{Representation of state}

The representation of a state in our interpreter is a structure containing the
following values: the step count (which will double as our timestamp, since
Pure Data has time-based execution); the state for each node; and the output,
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

\section{Evaluation model}

The execution mode of Pure Data is data-driven. The user triggers events via
its interface, and those events cause a cascading series of firings. The user
may trigger events by clicking nodes, entering numbers (or using MIDI devices,
which are equivalent to inputting numbers).

We represent events with a timestamp, the node index indicating which node was
triggered, and an optional atom representing data entered by the user.

\begin{code}

data PdEvent = PdEvent Int Int (Maybe PdAtom)
   deriving Show

\end{code}


\begin{code}

-- Produce a sequence of n empty inlets
emptyInlets :: Int -> Seq [PdAtom]
emptyInlets n = fromList (replicate n [])

ffor a f = fmap f a

initialState :: PdPatch -> Seq PdNodeState
initialState (PdPatch _ nodes conns _) =
   Data.Foldable.foldl doConn zeroState conns
   where
      zeroState :: Seq PdNodeState
      zeroState =
         ffor nodes (\ node ->
            case node of
               PdAtomBox     atoms    -> PdNodeState  (emptyInlets 1)    atoms
               PdObject      _ inl _  -> PdNodeState  (emptyInlets inl)  []
               PdMessageBox  _        -> PdNodeState  (emptyInlets 1)    []
         )
      doConn :: Seq PdNodeState -> PdConnection -> Seq PdNodeState
      doConn prev (PdConnection (src, outl) (dst, inl)) =
         let
            srcNode = index nodes src
            dstState@(PdNodeState dstInlets dstAtoms) = index prev dst
            dstInlet = index dstInlets inl
         in
            case srcNode of
               PdAtomBox atoms ->
                  let
                     newInlets = update inl (dstInlet ++ atoms) dstInlets
                     newState = PdNodeState newInlets dstAtoms
                  in
                     update dst newState prev
               _ ->
                  prev

dollarExpansion :: PdCommand -> [PdAtom] -> (PdReceiver, [PdAtom])
dollarExpansion cmd@(PdCommand recv tokens) cmdState =
   (recv', atoms')
   where
      recv' =
         case recv of
            PdRDollar n ->
               -- TODO test out-of-range (Pure Data gives an error)
               case (cmdState !! (trace (show n) n)) of
                  PdSymbol s  -> PdReceiver s
                  _           -> PdReceiverErr
            _ -> recv
      atoms' = 
         ffor tokens (\token ->
            case token of
               -- TODO test out-of-range (Pure Data gives an error)
               PdTDollar n   -> cmdState !! (trace (show cmdState ++ " !! " ++ show n) n)
               PdTAtom atom  -> atom
         )

updateNodeState :: Int -> PdNodeState -> PdState -> PdState
updateNodeState idx ns state@(PdState step nss output) =
   PdState step (update idx ns nss) output

updateInlet :: Int -> Int -> [PdAtom] -> PdState -> PdState
updateInlet dst inl atoms state@(PdState step nss output) =
   let
      oldState@(PdNodeState inlets internal) = index nss dst
      newState = PdNodeState (update inl atoms inlets) internal
   in
      updateNodeState dst newState state

saturate :: Functor f => f PdAtom -> f PdAtom
saturate =
   fmap (\(PdFloat f) -> PdFloat (max (-1.0) (min 1.0 f)))

-- additive synthesis
addToInlet :: Int -> Int -> [PdAtom] -> PdState -> PdState
addToInlet dst inl atoms state@(PdState step nss output) =
   let
      oldState@(PdNodeState inlets internal) = index nss dst
      oldAtoms = index inlets inl
      newAtoms = saturate $ fmap (\(PdFloat a, PdFloat b) -> PdFloat (a + b)) (zip oldAtoms atoms)
      newState = PdNodeState (update inl newAtoms inlets) internal
   in
      updateNodeState dst newState state

printOut :: [PdAtom] -> PdState -> PdState
printOut atoms state@(PdState step nss output) =
   PdState step nss (output ++ ["print: " ++ show atoms])

zeroDspInlets :: PdState -> [Int] -> PdState
zeroDspInlets state@(PdState step nss output) dspSort =
   let
      nss' =
         fromList $ clearedStates 0 (toList nss) (sort dspSort)
            where
               zeroState :: PdNodeState -> PdNodeState
               zeroState (PdNodeState inlets internal) =
                  PdNodeState (fromList $ replicate (Data.Sequence.length inlets) (replicate 64 (PdFloat 0.0))) internal

               clearedStates :: Int -> [PdNodeState] -> [Int] -> [PdNodeState]
               clearedStates i (st:sts) indices@(idx:idxs)
                  | i == idx   = zeroState st  : clearedStates (i+1) sts idxs
                  | otherwise  = st            : clearedStates (i+1) sts indices
               clearedStates i  nss  []  = nss
               clearedStates i  []      _   = []
   in
      PdState step nss' output

performDsp :: PdNode -> PdNodeState -> ([PdAtom], [PdAtom])

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
      (output, newInternal)

performDsp obj@(PdObject [PdSymbol "line~"] _ _) ns@(PdNodeState inlets []) =
   performDsp obj (PdNodeState inlets [PdFloat 0, PdFloat 0, PdFloat 0])

performDsp obj@(PdObject [PdSymbol "line~"] _ _) ns@(PdNodeState inlets [PdFloat current, PdFloat target, PdFloat delta]) =
   let
      limiter = if delta > 0 then min else max
      output = map PdFloat $ tail $ take 65 $ iterate (\v -> limiter target (v + delta)) current
      newInternal = [last output, PdFloat target, PdFloat delta]
   in
      (output, newInternal)

performDsp obj@(PdObject [PdSymbol "*~"] _ _) ns@(PdNodeState inlets []) =
   let
      mult (PdFloat a) (PdFloat b) = PdFloat (a * b)
      output = zipWith mult (index inlets 0) (index inlets 1)
   in
      (output, [])

performDsp obj ns =
   trace ("performDsp catch-all: " ++ show obj) (toList $ replicate 64 $ PdFloat 0.0, [])

run :: Int -> PdPatch -> [PdEvent] -> [PdState]
run steps patch@(PdPatch _ nodes conns dspSort) events =
   let

      sendMessage :: [PdAtom] -> PdNode -> Int -> Int -> PdState -> PdState
      
      -- message box:
      
      sendMessage atoms (PdMessageBox cmds) nodeIdx 0 state =
         foldl (processCommand nodeIdx) (updateInlet nodeIdx 0 atoms state) cmds

      -- "print" object:
      
      sendMessage (PdSymbol "float" : fs) (PdObject (PdSymbol "print" : xs) _ _) _ 0 state =
         printOut (xs ++ fs) state
      
      sendMessage atoms (PdObject (PdSymbol "print" : xs) _ _) _ 0 state =
         printOut (xs ++ atoms) state
      
      -- "float" object:
      
      sendMessage [PdSymbol "bang"] (PdObject (PdSymbol "float" : xs) _ _) nodeIdx 0 state@(PdState _ nss _) =
         let
            (PdNodeState inlets internal) = index nss nodeIdx
            oldInternal = if internal /= [] then internal else [PdFloat 0]
            inlet1 = index inlets 1
            newInternal = if inlet1 == [] then oldInternal else inlet1
            state' = updateNodeState nodeIdx (PdNodeState (emptyInlets (Data.Sequence.length inlets)) newInternal) state
         in
            trace ("outputting float " ++ show newInternal)
               (forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) state')

      sendMessage (PdSymbol "float" : fl) (PdObject [PdSymbol "float"] inl _) nodeIdx 0 state@(PdState _ nss _) =
         let
            state' = updateNodeState nodeIdx (PdNodeState (emptyInlets inl) fl) state
         in
            trace ("forwarding float " ++ show fl)
               (forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : fl)) state')
      
      -- "+" object:
      
      sendMessage [PdSymbol "float", fl] (PdObject [PdSymbol "+", n] _ _) nodeIdx 0 state@(PdState _ nss _) =
         let
            (PdNodeState inlets internal) = trace "+ got a float"
                                               (index nss nodeIdx)
            (PdFloat val0) = fl
            inlet1 = index inlets 1
            (PdFloat val1) = if inlet1 == [] then n else head inlet1
            newInternal = [PdFloat (val0 + val1)]
            state' = updateNodeState nodeIdx (PdNodeState (update 0 [fl] inlets) newInternal) state
         in
            forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) state'
      
      sendMessage [PdSymbol "bang"] (PdObject [PdSymbol "+", n] _ _) nodeIdx 0 state@(PdState _ nss _) =
         let
            (PdNodeState inlets internal) = index nss nodeIdx
            inlet0 = index inlets 0
            (PdFloat val0) = if inlet0 == [] then (PdFloat 0) else head inlet0
            inlet1 = index inlets 1
            (PdFloat val1) = if inlet1 == [] then n else head inlet1
            newInternal = [PdFloat (val0 + val1)]
            state' = updateNodeState nodeIdx (PdNodeState inlets newInternal) state
         in
            forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) state'

      -- "osc~" object:

      sendMessage [PdSymbol "float", PdFloat freq] (PdObject (PdSymbol "osc~" : _) _ _) nodeIdx 0 state@(PdState _ nss _) =
         let
            (PdNodeState inlets [_, position]) = index nss nodeIdx
         in
            updateNodeState nodeIdx (PdNodeState inlets [PdFloat ((2 * pi) / (32000 / freq)), position]) state

      -- "line~" object:
      
      sendMessage [PdSymbol "float", PdFloat amp, PdFloat time] (PdObject [PdSymbol "line~"] _ _) nodeIdx 0 state@(PdState _ nss _) =
         let
            (PdNodeState inlets internal) = index nss nodeIdx
            [PdFloat current, PdFloat target, PdFloat delta] = if internal /= [] then internal else [PdFloat 0, PdFloat 0, PdFloat 0]
            newInternal = [PdFloat current, PdFloat amp, PdFloat ((amp - current) / (time * 32))]
         in
            updateNodeState nodeIdx (PdNodeState inlets newInternal) state
      
      -- cold inlets:
      
      sendMessage (PdSymbol "float" : fs) node nodeIdx inl state =
         trace ("catch all float: " ++ show fs ++ " to " ++ show node ++ " " ++ show inl)
            (updateInlet nodeIdx inl fs state)

      sendMessage atoms node nodeIdx inl state =
         trace ("catch all: " ++ show atoms ++ " to " ++ show node ++ " " ++ show inl)
            (updateInlet nodeIdx inl atoms state)
      
      --

      forEachOutlet :: Int -> (PdNode -> Int -> Int -> PdState -> PdState) -> PdState -> PdState
      forEachOutlet idx op state =
         Data.Foldable.foldl handle state conns
         where
            handle :: PdState -> PdConnection -> PdState
            handle state (PdConnection (src, _) (dst, inl))
               | src == idx = (trace ("Will send event to "++show dst++" "++show inl) op (index nodes dst) dst inl state)
               | otherwise  = state

      forEachReceiver :: String -> (PdNode -> Int -> Int -> PdState -> PdState) -> PdState -> PdState
      forEachReceiver name op state =
         Data.Sequence.foldlWithIndex handle state nodes
         where
            handle :: PdState -> Int -> PdNode -> PdState
            handle state dst node@(PdObject (PdSymbol "r" : (PdSymbol rname : _)) _ _)
               | name == rname = forEachOutlet dst op state
            handle state _ _ = state

      normalizeMessage :: [PdAtom] -> [PdAtom]
      normalizeMessage atoms@(PdFloat f : xs) =
         (PdSymbol "float" : atoms)
      normalizeMessage atoms = atoms

      processCommand :: Int -> PdState -> PdCommand -> PdState
      processCommand idx state@(PdState step nss output) cmd =
         let
            (PdNodeState inlets _) = index nss idx
            inletData = index inlets 0
            (recv, atoms) = dollarExpansion cmd (trace ("data: "++show inletData) inletData)
         in
            case (trace ("At " ++ show idx ++ " routing " ++ show atoms ++ " to " ++ show recv) recv) of
               PdToOutlet ->
                  forEachOutlet idx (sendMessage (normalizeMessage atoms)) state
               PdReceiver r ->
                  forEachReceiver r (sendMessage (normalizeMessage atoms)) state
               PdReceiverErr ->
                  printOut [PdSymbol "$1: symbol needed as message destination"] state
   
      processEvent :: PdEvent -> PdState -> PdState
      processEvent event@(PdEvent time idx arg) state@(PdState step _ _) =
         let
            node = index nodes idx
         in
            case (trace ("New EVENT: " ++ show time ++ "/ object " ++ show idx ++ " for state " ++ show state) node) of
               PdMessageBox cmds -> updateInlet idx 0 [] (foldl (processCommand idx) state cmds)
               PdAtomBox _ ->
                  case arg of
                     Just atom -> updateInlet idx 0 [atom] (processCommand idx state (PdCommand PdToOutlet [PdTAtom atom]))
                     Nothing -> error "Events on atom boxes expect an argument."
               _ -> state

      processDspTree :: PdState -> PdState
      processDspTree state =
         foldl handle (zeroDspInlets state dspSort) dspSort
         where
            handle :: PdState -> Int -> PdState
            handle state'@(PdState step nss _) idx =
               let
                  obj = index nodes idx
                  ns@(PdNodeState inlets internal) = index nss idx
                  (outDsp, newInternal) = performDsp obj ns
                  state'' = updateNodeState idx (PdNodeState inlets newInternal) state'
               in
                  Data.Foldable.foldl (propagate outDsp) state'' conns
                  where
                     propagate :: [PdAtom] -> PdState -> PdConnection -> PdState
                     propagate outDsp state (PdConnection (src, 0) (dst, inl))
                        | src == idx = addToInlet dst inl outDsp state
                        | otherwise  = state
   
      runStep :: PdState -> [PdEvent] -> PdState
      runStep state@(PdState step _ _) events =
         let
            state'@(PdState _ nss output) = foldr processEvent state events
            state''@(PdState _ nss' output') =
               if step `mod` 2 == 0
               then processDspTree state'
               else state'
         in
            PdState (step + 1) nss' output'
      
      loop :: PdState -> [PdEvent] -> [PdState] -> [PdState]
      loop state@(PdState step _ _) events states =
         if step == steps
         then states
         else
            let (currEvents, nextEvents) = span (\(PdEvent time _ _) -> time == step) events
            in loop (runStep state currEvents) nextEvents (states ++ [state])

   in loop (PdState 0 (initialState patch) []) events []

cSharp = 554.37
aSharp = 932.33
g = 783.99
gSharp = 830.61
f = 698.46

-- osc0.pd
patch = PdPatch 10 (fromList [
            PdAtomBox    [PdFloat 0], -- [PdControlInlet True "bang"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "osc~", PdFloat gSharp] 2 1, -- [PdControlInlet True "float", PdControlInlet True "float"] [PdSignalOutlet],
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdFloat 0.5), PdTAtom (PdFloat 1000)]], -- [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdFloat 0), PdTAtom (PdFloat 100)]], -- [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdObject     [PdSymbol "line~"] 2 1, -- [PdControlInlet True "list", PdControlInlet False "float"] [PdSignalOutlet],
            PdObject     [PdSymbol "*~"] 2 1, -- [PdSignalInlet 0, PdSignalInlet 0] [PdSignalOutlet],
            PdObject     [PdSymbol "dac~"] 1 0, -- [PdSignalInlet 0] [],
            PdObject     [PdSymbol "r", PdSymbol "metroToggle"] 0 1, -- [] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "metro", PdFloat 500] 1 1, --  [PdControlInlet True "bang"] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "tabwrite~", PdSymbol "array99"] 1 0, -- [PdControlInlet True "signal"] [],
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

getData :: PdNodeState -> [Integer]
getData ns@(PdNodeState inlets _) =
   let
      inlet = index inlets 0
   in
      map (\(PdFloat f) -> floor (((f + 1) / 2) * 65535)) inlet

everyOther :: [a] -> [a]
everyOther (x:(y:xs)) = x : everyOther xs
everyOther x = x

genOutput = concat . everyOther . toList . fmap (\state@(PdState _ nss _) -> getData $ index nss 6)
--genOutput x = x


main :: IO ()
main = print (genOutput $ run 10000 patch [
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
