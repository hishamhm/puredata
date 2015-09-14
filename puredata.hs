
import Data.Sequence (Seq, fromList, index, update)
import qualified Data.Sequence(length, foldlWithIndex)
import Data.Foldable (toList)
import qualified Data.Foldable (foldl)
import Debug.Trace

data PdAtom = PdFloat Float
            | PdSymbol String
            | PdGPointer Int
   deriving (Show, Eq)

type PdSample = Float

type PdSignal = [PdSample]

data PdInlet = PdControlInlet Bool String
             | PdSignalInlet Float
   deriving Show

data PdOutlet = PdControlOutlet String
              | PdSignalOutlet
   deriving Show

data PdToken = PdTDollar Int
             | PdTAtom PdAtom
   deriving Show

data PdReceiver = PdToOutlet
                | PdReceiver String
                | PdRDollar Int
                | PdReceiverErr
   deriving Show

data PdCommand = PdCommand PdReceiver [PdToken]
   deriving Show

              -- creation-arguments, inlets, outlets
data PdNode = PdObject [PdAtom] [PdInlet] [PdOutlet]
            | PdAtomBox [PdAtom] [PdInlet] [PdOutlet]
            | PdMessageBox [PdCommand] [PdInlet] [PdOutlet]
            | PdGObject [PdAtom] [PdInlet] [PdOutlet]
   deriving Show

data PdMessage = PdMessage [PdAtom]
   deriving Show

                    -- (src-node-idx, outlet-idx), (dst-node-idx, inlet-idx)
data PdConnection = PdConnection (Int, Int) (Int, Int)

               -- time, node-idx
data PdEvent = PdEvent Int Int
   deriving Show

type PdBufferSize = Int

data PdPatch = PdPatch PdBufferSize (Seq PdNode) (Seq PdConnection)

                   -- inlet-data, internal-state
data PdNodeState = PdNodeState (Seq [PdAtom]) [PdAtom]

instance Show PdNodeState where
   show (PdNodeState inlets atoms) =
      "{" ++ show (toList inlets) ++ " | " ++ (show atoms) ++ "}\n"

             -- step, node-states, outputs
data PdEnv = PdEnv Int (Seq PdNodeState) [String]

instance Show PdEnv where
   show (PdEnv step states output) =
      "Step: " ++ show step ++ " - Nodes: " ++ show (toList states) ++ " - Output: " ++ concatMap (\l -> l ++ "\n") output ++ "\n"

-- Produce a sequence of n empty inlets
emptyInlets :: Int -> Seq [PdAtom]
emptyInlets n = fromList (replicate n [])

ffor a f = fmap f a

initialState :: PdPatch -> Seq PdNodeState
initialState (PdPatch _ nodes conns) =
   Data.Foldable.foldl doConn zeroState conns
   where
      zeroState :: Seq PdNodeState
      zeroState =
         ffor nodes (\ node ->
            case node of
               PdAtomBox    atoms inl _ -> PdNodeState (emptyInlets (length inl)) atoms
               PdObject     _     inl _ -> PdNodeState (emptyInlets (length inl)) []
               PdMessageBox _     inl _ -> PdNodeState (emptyInlets (length inl)) []
               PdGObject    _     inl _ -> PdNodeState (emptyInlets (length inl)) []
         )
      doConn :: Seq PdNodeState -> PdConnection -> Seq PdNodeState
      doConn prev (PdConnection (src, outl) (dst, inl)) =
         let
            srcNode = index nodes src
            dstState@(PdNodeState dstInlets dstAtoms) = index prev dst
            dstInlet = index dstInlets inl
         in
            case srcNode of
               PdAtomBox atoms _ _ ->
                  let
                     updDstInlets = update inl (dstInlet ++ atoms) dstInlets
                     updDstState = PdNodeState updDstInlets dstAtoms
                  in
                     update dst updDstState prev
               _ ->
                  prev

dollarExpansion :: PdCommand -> [PdAtom] -> (PdReceiver, [PdAtom])
dollarExpansion cmd@(PdCommand recv tokens) cmdState =
   (recv', atoms')
   where
      recv' =
         case recv of
            PdRDollar n ->
               case (cmdState !! (trace (show n) n)) of
                  PdSymbol s -> PdReceiver s
                  _          -> PdReceiverErr
            _ -> recv
      atoms' = 
         ffor tokens (\token ->
            case token of
               PdTDollar n -> cmdState !! (trace (show cmdState ++ " !! " ++ show n) n)
               PdTAtom atom -> atom
         )

updateNodeState :: Int -> PdNodeState -> PdEnv -> PdEnv
updateNodeState dst state env@(PdEnv step states output) =
   PdEnv step (update dst state states) output

updateInlet :: Int -> Int -> [PdAtom] -> PdEnv -> PdEnv
updateInlet dst inl atoms env@(PdEnv step states output) =
   let
      oldState@(PdNodeState inlets internal) = index states dst
      newState = PdNodeState (update inl atoms inlets) internal
   in
      updateNodeState dst newState env

printOut :: [PdAtom] -> PdEnv -> PdEnv
printOut atoms env@(PdEnv step states output) =
   PdEnv step states (output ++ ["print: " ++ show atoms])

run :: Int -> PdPatch -> [PdEvent] -> [PdEnv]
run steps patch@(PdPatch _ nodes conns) events = 
   let

      sendMessage :: [PdAtom] -> PdNode -> Int -> Int -> PdEnv -> PdEnv
      
      -- message box:
      
      sendMessage atoms (PdMessageBox cmds _ _) nodeIdx 0 env =
         foldl (processCommand nodeIdx) (updateInlet nodeIdx 0 atoms env) cmds
      
      -- "print" object:
      
      sendMessage (PdSymbol "float" : fs) (PdObject (PdSymbol "print" : xs) _ _) _ 0 env =
         printOut (xs ++ fs) env
      
      sendMessage atoms (PdObject (PdSymbol "print" : xs) _ _) _ 0 env =
         printOut (xs ++ atoms) env
      
      -- "float" object:
      
      sendMessage [PdSymbol "bang"] (PdObject (PdSymbol "float" : xs) _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState inlets internal) = index states nodeIdx
            oldInternal = if internal /= [] then internal else [PdFloat 0]
            inlet1 = index inlets 1
            newInternal = if inlet1 == [] then oldInternal else inlet1
            env' = updateNodeState nodeIdx (PdNodeState (emptyInlets (Data.Sequence.length inlets)) newInternal) env
         in
            trace ("outputting float " ++ show newInternal)
               (forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) env')

      sendMessage (PdSymbol "float" : fl) (PdObject [PdSymbol "float"] inl _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            env' = updateNodeState nodeIdx (PdNodeState (emptyInlets (length inl)) fl) env
         in
            trace ("forwarding float " ++ show fl)
               (forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : fl)) env')
      
      -- "+" object:
      
      sendMessage [PdSymbol "float", (PdFloat f)] (PdObject [PdSymbol "+", n] _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState inlets internal) = trace "+ got a float"
                                               (index states nodeIdx)
            inlet1 = index inlets 1
            (PdFloat incVal) = if inlet1 == [] then n else head inlet1
            newInternal = [PdFloat (f + incVal)]
            env' = updateNodeState nodeIdx (PdNodeState inlets newInternal) env
         in
            forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) env'
      
      sendMessage [PdSymbol "bang"] (PdObject [PdSymbol "+", n] _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState _ internal) = index states nodeIdx
         in
            forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : internal)) env
      
      -- cold inlets:
      
      sendMessage (PdSymbol "float" : fs) node nodeIdx inl env =
         trace ("catch all float: " ++ show fs ++ " to " ++ show node ++ " " ++ show inl)
            (updateInlet nodeIdx inl fs env)

      sendMessage atoms node nodeIdx inl env =
         trace ("catch all: " ++ show atoms ++ " to " ++ show node ++ " " ++ show inl)
            (updateInlet nodeIdx inl atoms env)
      
      --

      forEachOutlet :: Int -> (PdNode -> Int -> Int -> PdEnv -> PdEnv) -> PdEnv -> PdEnv
      forEachOutlet idx op env =
         Data.Foldable.foldl handle env conns
         where
            handle :: PdEnv -> PdConnection -> PdEnv
            handle env (PdConnection (src, _) (dst, inl)) =
               if src == idx
               then (trace ("Will send event to "++show dst++" "++show inl) op (index nodes dst) dst inl env)
               else env

      forEachReceiver :: String -> (PdNode -> Int -> Int -> PdEnv -> PdEnv) -> PdEnv -> PdEnv
      forEachReceiver name op env =
         Data.Sequence.foldlWithIndex handle env nodes
         where
            handle :: PdEnv -> Int -> PdNode -> PdEnv
            handle env dst node@(PdObject (PdSymbol "r" : (PdSymbol rname : _)) _ _) =
               if name == rname
               then forEachOutlet dst op env
               else env
            handle env _ _ = env

      processCommand :: Int -> PdEnv -> PdCommand -> PdEnv
      processCommand idx env@(PdEnv step states output) cmd =
         let
            (PdNodeState inlets _) = index states idx
            inletData = index inlets 0
            (recv, atoms) = dollarExpansion cmd (trace ("data: "++show inletData) inletData)
         in
            case (trace ("At " ++ show idx ++ " routing " ++ show atoms ++ " to " ++ show recv ++ " " ++ show idx) recv) of
               PdToOutlet ->
                  forEachOutlet idx (sendMessage atoms) env
               PdReceiver r ->
                  forEachReceiver r (sendMessage atoms) env
               PdReceiverErr ->
                  printOut [PdSymbol "$1: symbol needed as message destination"] env

   
      processEvent :: PdEvent -> PdEnv -> PdEnv
      processEvent event@(PdEvent time idx) env@(PdEnv step _ _) =
         let
            node = index nodes idx
         in
            case (trace ("New EVENT: " ++ show time ++ "/" ++ show idx ++ " for env " ++ show env) node) of
               PdMessageBox cmds _ _ -> updateInlet idx 0 [] (foldl (processCommand idx) env cmds)
               _ -> env
   
      runStep :: PdEnv -> [PdEvent] -> PdEnv
      runStep env events =
         let
            env'@(PdEnv step states output) = foldr processEvent env events
         in
            PdEnv (step + 1) states output --FIXME
      
      loop :: PdEnv -> [PdEvent] -> [PdEnv] -> [PdEnv]
      loop env@(PdEnv step states output) events envs =
         if step == steps
         then envs
         else
            let (currEvents, nextEvents) = span (\(PdEvent time _) -> time == step) events
            in loop (runStep env currEvents) nextEvents (envs ++ [env])

   in loop (PdEnv 0 (initialState patch) []) events []

{-
-- osc.pd
patch = PdPatch 10 (fromList [
            PdAtomBox    [PdFloat 0] [PdControlInlet True "bang"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "osc~", PdFloat 1000] [PdControlInlet True "float", PdControlInlet True "float"] [PdSignalOutlet],
            PdAtomBox    [PdFloat 0.1, PdFloat 100] [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdAtomBox    [PdFloat 0, PdFloat 100] [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdObject     [PdSymbol "line~", PdFloat 0] [PdControlInlet True "list", PdControlInlet False "float"] [PdSignalOutlet],
            PdObject     [PdSymbol "*~", PdFloat 0] [PdSignalInlet 0, PdSignalInlet 0] [PdSignalOutlet],
            PdObject     [PdSymbol "dac~", PdFloat 0] [PdSignalInlet 0] [],
            PdObject     [PdSymbol "r", PdSymbol "metroToggle"] [] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "metro", PdFloat 500] [PdControlInlet True "bang"] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "tabwrite~", PdSymbol "array99"] [PdControlInlet True "signal"] [],
            PdGObject    [PdSymbol "array99"] [] [],
            PdMessageBox [PdCommand [], PdCommand [PdTAtom (PdSymbol "metroToggle"), PdTAtom (PdFloat 1.0)]] [] [],
            PdMessageBox [PdCommand [], PdCommand [PdTAtom (PdSymbol "metroToggle"), PdTAtom (PdFloat 0.0)]] [] []
         ]) (fromList [
            PdConnection (0, 0) (1, 0), -- 0 -> osc~
            PdConnection (1, 0) (5, 0), -- osc~ -> *~
            PdConnection (1, 0) (9, 0), -- osc~ -> tabwrite~
            PdConnection (7, 0) (8, 0), -- r -> metro
            PdConnection (8, 0) (9, 0), -- metro -> tabwrite~
            PdConnection (2, 0) (4, 0), -- 0.1 -> line~
            PdConnection (3, 0) (4, 0), -- 0 -> line~
            PdConnection (4, 0) (5, 1), -- line~ -> *~
            PdConnection (5, 0) (6, 0)  -- line~ -> dac~
         ])
-}

-- messages.pd
{-
patch = PdPatch 10 (fromList [
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 1), PdTAtom (PdFloat 2)], PdCommand PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 10), PdTAtom (PdFloat 20)]] [PdControlInlet True "list"] [], 
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdSymbol "foo"), PdTAtom (PdFloat 5), PdTAtom (PdFloat 6)]] [PdControlInlet True "list"] [], 
            PdMessageBox [PdCommand PdToOutlet [PdTDollar 1, PdTDollar 1], PdCommand (PdRDollar 1) [PdTDollar 2], PdCommand (PdReceiver "bar") [PdTDollar 2, PdTDollar 1]] [PdControlInlet True "list"] [], 
            PdObject     [PdSymbol "print"] [PdControlInlet True "symbol"] [],
            PdObject     [PdSymbol "print"] [PdControlInlet True "symbol"] [],
            PdObject     [PdSymbol "r", PdSymbol "foo"] [] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "print", PdSymbol "viaFoo"] [PdControlInlet True "symbol"] [],
            PdObject     [PdSymbol "r", PdSymbol "bar"] [] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "print", PdSymbol "viaBar"] [PdControlInlet True "symbol"] []
         ]) (fromList [
            PdConnection (0, 0) (2, 0), -- 1 2, 10 20 -> $1 $1...
            PdConnection (1, 0) (2, 0), -- 4 5 6 -> $1 $1...
            PdConnection (2, 0) (3, 0), -- $1 $1... -> print
            PdConnection (2, 0) (4, 0), -- $1 $1... -> print
            PdConnection (5, 0) (6, 0), -- r foo -> print viaFoo
            PdConnection (7, 0) (8, 0)  -- r bar -> print viaBar
         ])

main :: IO ()
main = print (run 30 patch [(PdEvent 1 0), (PdEvent 3 1)])
-}

-- inc.pd
patch :: PdPatch
patch = PdPatch 10 (fromList [
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "bang")]] [PdControlInlet True "list"] [],
            PdObject     [PdSymbol "float"] [PdControlInlet True "float", PdControlInlet False "float"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "+", PdFloat 1] [PdControlInlet True "float", PdControlInlet False "float"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "print"] [PdControlInlet True "list"] [],

            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "bang")]] [PdControlInlet True "list"] [],
            PdMessageBox [PdCommand PdToOutlet [PdTAtom (PdSymbol "float"), PdTAtom (PdFloat 100)]] [PdControlInlet True "list"] [],
            PdObject     [PdSymbol "print"] [PdControlInlet True "list"] []

         ]) (fromList [
            PdConnection (0, 0) (1, 0), -- bang -> float
            PdConnection (1, 0) (2, 0), -- float -> + 1
            PdConnection (2, 0) (1, 1), -- + 1 -> float
            PdConnection (1, 0) (3, 0), -- float -> print
            
            PdConnection (4, 0) (2, 0), -- bang -> + 1
            PdConnection (5, 0) (2, 1), -- 100 -> + 1
            PdConnection (2, 0) (6, 0)  -- + 1 -> print
         ])

main :: IO ()
main = print (run 30 patch [(PdEvent 1 0), (PdEvent 3 0), (PdEvent 5 0), (PdEvent 6 4), (PdEvent 7 4), (PdEvent 8 4), (PdEvent 10 5),  (PdEvent 12 0), (PdEvent 13 0), (PdEvent 15 0) ])
--
