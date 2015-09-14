
import Data.Sequence (Seq, fromList, index, update, foldlWithIndex)
import qualified Data.Sequence (length)
import Data.Foldable (toList)
import qualified Data.Foldable (foldl)
import Debug.Trace

data PdAtom = PdFloat Float
            | PdSymbol String
            | PdGPointer Int
   deriving Show

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

data PdReceiver = PdOutlet
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

ffor a f = fmap f a

initialState :: PdPatch -> Seq PdNodeState
initialState (PdPatch _ nodes conns) =
   Data.Foldable.foldl doConn zeroState conns
   where
      zeroState :: Seq PdNodeState
      zeroState =
         ffor nodes (\ node ->
            let
               -- Fill every inlet with an empty list
               emptyInlets :: [PdInlet] -> Seq [PdAtom]
               emptyInlets inl = fromList (replicate (length inl) [])
            in
               case node of
                  PdAtomBox    atoms inl _ -> PdNodeState (emptyInlets inl) atoms
                  PdObject     _     inl _ -> PdNodeState (emptyInlets inl) []
                  PdMessageBox _     inl _ -> PdNodeState (emptyInlets inl) []
                  PdGObject    _     inl _ -> PdNodeState (emptyInlets inl) []
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

updateEnv :: Int -> Int -> [PdAtom] -> PdEnv -> PdEnv
updateEnv dst inl atoms env@(PdEnv step states output) =
   let
      oldState@(PdNodeState inlets internal) = index states dst
      newState = PdNodeState (update inl atoms inlets) internal
      newStates = update dst newState states
   in
      PdEnv step newStates output

printOut :: [PdAtom] -> PdEnv -> PdEnv
printOut atoms env@(PdEnv step states output) =
   PdEnv step states (output ++ ["print: " ++ show atoms])

run :: Int -> PdPatch -> [PdEvent] -> [PdEnv]
run steps patch@(PdPatch _ nodes conns) events = 
   let
   
      sendMessage :: [PdAtom] -> Int -> Int -> PdEnv -> PdEnv
      sendMessage atoms idx inl env@(PdEnv step states output) =
         let
            node = index nodes idx
            state@(PdNodeState inlets _) = index states idx
            env' = (updateEnv idx 0 atoms env)
         in
            if (trace ("sendMessage atoms " ++ show atoms ++ " to " ++ show idx) (inl == 0))
            then
               let 
                  env'' = 
                     case node of
                        PdObject (PdSymbol "print" : xs) _ _ -> printOut (xs ++ atoms) env'
                        PdMessageBox cmds _ _ -> (foldl (processCommand idx) env' cmds)
                        _ -> env' --FIXME
               in
                  updateEnv idx 0 [] env''
            else updateEnv idx inl atoms env
   
      forEachOutlet idx op env =
         Data.Foldable.foldl handle env conns
         where
            handle :: PdEnv -> PdConnection -> PdEnv
            handle env (PdConnection (src, _) (dst, inl)) =
               if src == idx
               then (trace ("Will send event to "++show dst++" "++show inl) op dst inl env)
               else env

      forEachReceiver :: String -> (Int -> Int -> PdEnv -> PdEnv) -> PdEnv -> PdEnv
      forEachReceiver name op env =
         foldlWithIndex handle env nodes
         where
            handle :: PdEnv -> Int -> PdNode -> PdEnv
            handle env dst (PdObject (PdSymbol "r" : (PdSymbol rname : _)) _ _) =
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
            case (trace ("Routing " ++ show atoms ++ " to " ++ show recv ++ " " ++ show idx) recv) of
               PdOutlet ->
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
            case (trace ("New EVENT: " ++ show time ++ "/" ++ show idx) node) of
               PdMessageBox cmds _ _ -> updateEnv idx 0 [] (foldl (processCommand idx) env cmds)
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
patch = PdPatch 10 (fromList [
            PdMessageBox [PdCommand PdOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 1), PdTAtom (PdFloat 2)], PdCommand PdOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 10), PdTAtom (PdFloat 20)]] [PdControlInlet True "list"] [], 
            PdMessageBox [PdCommand PdOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdSymbol "foo"), PdTAtom (PdFloat 5), PdTAtom (PdFloat 6)]] [PdControlInlet True "list"] [], 
            PdMessageBox [PdCommand PdOutlet [PdTDollar 1, PdTDollar 1], PdCommand (PdRDollar 1) [PdTDollar 2], PdCommand (PdReceiver "bar") [PdTDollar 2, PdTDollar 1]] [PdControlInlet True "list"] [], 
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
