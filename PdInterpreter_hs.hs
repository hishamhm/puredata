import Data.Sequence (Seq, fromList, index, update)
import qualified Data.Sequence (length, foldlWithIndex)
import Data.Foldable (toList)
import qualified Data.Foldable (foldl)
import Debug.Trace
import Data.List (sort)
import Text.Printf
import Data.Fixed
import Data.Binary.Put
import qualified Data.ByteString.Lazy as ByteString
import Control.Monad

data PdAtom = PdFloat Double
            | PdSymbol String
            | PdGPointer Int
   deriving Eq

instance Show PdAtom where
   show (PdFloat f) = show f
   show (PdSymbol s) = show s
   show (PdGPointer i) = "GPointer:" ++ show i

type PdSample = Double

type PdSignal = [PdSample]

data PdInlet = PdControlInlet Bool String
             | PdSignalInlet Double
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

data PdCmd = PdCmd PdReceiver [PdToken]
   deriving Show

              -- creation-arguments, inlets, outlets
data PdNode = PdObject [PdAtom] [PdInlet] [PdOutlet]
            | PdAtomBox [PdAtom] [PdInlet] [PdOutlet]
            | PdMessageBox [PdCmd] [PdInlet] [PdOutlet]
            | PdGObject [PdAtom] [PdInlet] [PdOutlet]
   deriving Show

data PdMessage = PdMessage [PdAtom]
   deriving Show

                    -- (src-node-idx, outlet-idx), (dst-node-idx, inlet-idx)
data PdConnection = PdConnection (Int, Int) (Int, Int)
   deriving Show

               -- time, node-idx, arg
data PdEvent = PdEvent Int Int (Maybe PdAtom)
   deriving Show

type PdBufferSize = Int

               -- buffer-size, nodes, connections, dsp-sort-order
data PdPatch = PdPatch PdBufferSize (Seq PdNode) (Seq PdConnection) [Int]

                   -- inlet-data, internal-state
data PdNodeState = PdNodeState (Seq [PdAtom]) [PdAtom]

instance Show PdNodeState where
   show (PdNodeState inlets atoms) =
      "{" ++ show (toList inlets) ++ " | " ++ (show atoms) ++ "}\n"

             -- step, node-states, outputs
data PdEnv = PdEnv Int (Seq PdNodeState) [String]

instance Show PdEnv where
   show (PdEnv step states output) =
      "Step: " ++ show step ++ " - Nodes: " ++ show (toList states) ++ " - Output: {\n" ++ concatMap (\l -> l ++ "\n") output ++ "}\n"

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
                     newInlets = update inl (dstInlet ++ atoms) dstInlets
                     newState = PdNodeState newInlets dstAtoms
                  in
                     update dst newState prev
               _ ->
                  prev

dollarExpansion :: PdCmd -> [PdAtom] -> (PdReceiver, [PdAtom])
dollarExpansion cmd@(PdCmd recv tokens) cmdState =
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
updateNodeState idx state env@(PdEnv step states output) =
   PdEnv step (update idx state states) output

updateInlet :: Int -> Int -> [PdAtom] -> PdEnv -> PdEnv
updateInlet dst inl atoms env@(PdEnv step states output) =
   let
      oldState@(PdNodeState inlets internal) = index states dst
      newState = PdNodeState (update inl atoms inlets) internal
   in
      updateNodeState dst newState env

saturate :: Functor f => f PdAtom -> f PdAtom
saturate =
   fmap (\(PdFloat f) -> PdFloat (max (-1.0) (min 1.0 f)))

-- additive synthesis
addToInlet :: Int -> Int -> [PdAtom] -> PdEnv -> PdEnv
addToInlet dst inl atoms env@(PdEnv step states output) =
   let
      oldState@(PdNodeState inlets internal) = index states dst
      oldAtoms = index inlets inl
      newAtoms = saturate $ fmap (\(PdFloat a, PdFloat b) -> PdFloat (a + b)) (zip oldAtoms atoms)
      newState = PdNodeState (update inl newAtoms inlets) internal
   in
      updateNodeState dst newState env

printOut :: [PdAtom] -> PdEnv -> PdEnv
printOut atoms env@(PdEnv step states output) =
   PdEnv step states (output ++ ["print: " ++ show atoms])

zeroDspInlets :: PdEnv -> [Int] -> PdEnv
zeroDspInlets env@(PdEnv step states output) dspSort =
   let
      states' =
         fromList $ clearedStates 0 (toList states) (sort dspSort)
            where
               zeroState :: PdNodeState -> PdNodeState
               zeroState (PdNodeState inlets internal) =
                  PdNodeState (fromList $ replicate (Data.Sequence.length inlets) (replicate 64 (PdFloat 0.0))) internal

               clearedStates :: Int -> [PdNodeState] -> [Int] -> [PdNodeState]
               clearedStates i (st:sts) indices@(idx:idxs)
                  | i == idx  = zeroState st : clearedStates (i+1) sts idxs
                  | otherwise = st           : clearedStates (i+1) sts indices
               clearedStates i states [] = states
               clearedStates i []     _  = []
   in
      PdEnv step states' output

performDsp :: PdNode -> PdNodeState -> ([PdAtom], [PdAtom])

osc :: Double -> Double -> Double -> Double
osc position delta idx = (position + (delta * idx)) `mod'` (2 * pi)

performDsp obj@(PdObject [PdSymbol "osc~", PdFloat freq] _ _) state@(PdNodeState inlets []) =
   performDsp obj (PdNodeState inlets [PdFloat ((2 * pi) / (32000 / freq)), PdFloat 0])

performDsp obj@(PdObject [PdSymbol "osc~", _] _ _) state@(PdNodeState inlets [PdFloat delta, PdFloat position]) =
   let
      output = map (PdFloat . sin . osc position delta) [0..63]
      nextPosition = osc position delta 64
      newInternal = [PdFloat delta, PdFloat nextPosition]
   in
      (output, newInternal)

performDsp obj@(PdObject [PdSymbol "line~"] _ _) state@(PdNodeState inlets []) =
   performDsp obj (PdNodeState inlets [PdFloat 0, PdFloat 0, PdFloat 0])

performDsp obj@(PdObject [PdSymbol "line~"] _ _) state@(PdNodeState inlets [PdFloat current, PdFloat target, PdFloat delta]) =
   let
      limiter = if delta > 0 then min else max
      output = map PdFloat $ tail $ take 65 $ iterate (\v -> limiter target (v + delta)) current
      newInternal = [last output, PdFloat target, PdFloat delta]
   in
      (output, newInternal)

performDsp obj@(PdObject [PdSymbol "*~"] _ _) state@(PdNodeState inlets []) =
   let
      mult (PdFloat a) (PdFloat b) = PdFloat (a * b)
      output = zipWith mult (index inlets 0) (index inlets 1)
   in
      (output, [])

performDsp obj state =
   trace ("performDsp catch-all: " ++ show obj) (toList $ replicate 64 $ PdFloat 0.0, [])

run :: Int -> PdPatch -> [PdEvent] -> [PdEnv]
run steps patch@(PdPatch _ nodes conns dspSort) events =
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
      
      sendMessage [PdSymbol "float", fl] (PdObject [PdSymbol "+", n] _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState inlets internal) = trace "+ got a float"
                                               (index states nodeIdx)
            (PdFloat val0) = fl
            inlet1 = index inlets 1
            (PdFloat val1) = if inlet1 == [] then n else head inlet1
            newInternal = [PdFloat (val0 + val1)]
            env' = updateNodeState nodeIdx (PdNodeState (update 0 [fl] inlets) newInternal) env
         in
            forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) env'
      
      sendMessage [PdSymbol "bang"] (PdObject [PdSymbol "+", n] _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState inlets internal) = index states nodeIdx
            inlet0 = index inlets 0
            (PdFloat val0) = if inlet0 == [] then (PdFloat 0) else head inlet0
            inlet1 = index inlets 1
            (PdFloat val1) = if inlet1 == [] then n else head inlet1
            newInternal = [PdFloat (val0 + val1)]
            env' = updateNodeState nodeIdx (PdNodeState inlets newInternal) env
         in
            forEachOutlet nodeIdx (sendMessage (PdSymbol "float" : newInternal)) env'

      -- "osc~" object:

      sendMessage [PdSymbol "float", PdFloat freq] (PdObject (PdSymbol "osc~" : _) _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState inlets [_, position]) = index states nodeIdx
         in
            updateNodeState nodeIdx (PdNodeState inlets [PdFloat ((2 * pi) / (32000 / freq)), position]) env

      -- "line~" object:
      
      sendMessage [PdSymbol "list", PdFloat amp, PdFloat time] (PdObject [PdSymbol "line~"] _ _) nodeIdx 0 env@(PdEnv _ states _) =
         let
            (PdNodeState inlets internal) = index states nodeIdx
            [PdFloat current, PdFloat target, PdFloat delta] = if internal /= [] then internal else [PdFloat 0, PdFloat 0, PdFloat 0]
            newInternal = [PdFloat current, PdFloat amp, PdFloat ((amp - current) / (time * 32))]
         in
            updateNodeState nodeIdx (PdNodeState inlets newInternal) env
      
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
            handle env (PdConnection (src, _) (dst, inl))
               | src == idx = (trace ("Will send event to "++show dst++" "++show inl) op (index nodes dst) dst inl env)
               | otherwise  = env

      forEachReceiver :: String -> (PdNode -> Int -> Int -> PdEnv -> PdEnv) -> PdEnv -> PdEnv
      forEachReceiver name op env =
         Data.Sequence.foldlWithIndex handle env nodes
         where
            handle :: PdEnv -> Int -> PdNode -> PdEnv
            handle env dst node@(PdObject (PdSymbol "r" : (PdSymbol rname : _)) _ _)
               | name == rname = forEachOutlet dst op env
            handle env _ _ = env

      normalizeMessage :: [PdAtom] -> [PdAtom]
      normalizeMessage atoms@[PdFloat f]   = (PdSymbol "float" : atoms)
      normalizeMessage atoms@(PdFloat f : xs) = (PdSymbol "list"  : atoms)
      normalizeMessage atoms = atoms

      processCommand :: Int -> PdEnv -> PdCmd -> PdEnv
      processCommand idx env@(PdEnv step states output) cmd =
         let
            (PdNodeState inlets _) = index states idx
            inletData = index inlets 0
            (recv, atoms) = dollarExpansion cmd (trace ("data: "++show inletData) inletData)
         in
            case (trace ("At " ++ show idx ++ " routing " ++ show atoms ++ " to " ++ show recv) recv) of
               PdToOutlet ->
                  forEachOutlet idx (sendMessage (normalizeMessage atoms)) env
               PdReceiver r ->
                  forEachReceiver r (sendMessage (normalizeMessage atoms)) env
               PdReceiverErr ->
                  printOut [PdSymbol "$1: symbol needed as message destination"] env
   
      processEvent :: PdEvent -> PdEnv -> PdEnv
      processEvent event@(PdEvent time idx arg) env@(PdEnv step _ _) =
         let
            node = index nodes idx
         in
            case (trace ("New EVENT: " ++ show time ++ "/ object " ++ show idx ++ " for env " ++ show env) node) of
               PdMessageBox cmds _ _ -> updateInlet idx 0 [] (foldl (processCommand idx) env cmds)
               PdAtomBox _ _ _ ->
                  case arg of
                     Just atom -> updateInlet idx 0 [atom] (processCommand idx env (PdCmd PdToOutlet [PdTAtom atom]))
                     Nothing -> error "Events on atom boxes expect an argument."
               _ -> env

      processDspTree :: PdEnv -> PdEnv
      processDspTree env =
         foldl handle (zeroDspInlets env dspSort) dspSort
         where
            handle :: PdEnv -> Int -> PdEnv
            handle env'@(PdEnv step states _) idx =
               let
                  obj = index nodes idx
                  state@(PdNodeState inlets internal) = index states idx
                  (outDsp, newInternal) = performDsp obj state
                  env'' = updateNodeState idx (PdNodeState inlets newInternal) env'
               in
                  Data.Foldable.foldl (propagate outDsp) env'' conns
                  where
                     propagate :: [PdAtom] -> PdEnv -> PdConnection -> PdEnv
                     propagate outDsp env (PdConnection (src, 0) (dst, inl))
                        | src == idx = addToInlet dst inl outDsp env
                        | otherwise  = env
   
      runStep :: PdEnv -> [PdEvent] -> PdEnv
      runStep env@(PdEnv step _ _) events =
         let
            env'@(PdEnv _ states output) = foldr processEvent env events
            env''@(PdEnv _ states' output') =
               if step `mod` 2 == 0
               then processDspTree env'
               else env'
         in
            PdEnv (step + 1) states' output'
      
      loop :: PdEnv -> [PdEvent] -> [PdEnv] -> [PdEnv]
      loop env@(PdEnv step _ _) events envs =
         if step > steps
         then envs
         else
            let (currEvents, nextEvents) = span (\(PdEvent time _ _) -> time == step) events
            in loop (runStep env currEvents) nextEvents (envs ++ [env])

   in loop (PdEnv 0 (initialState patch) []) events []

getData :: PdNodeState -> [Integer]
getData state@(PdNodeState inlets _) =
   let
      inlet = index inlets 0
   in
      map (\(PdFloat f) -> floor (f * 65535)) inlet

everyOther :: [a] -> [a]
everyOther (x:(y:xs)) = x : everyOther xs
everyOther x = x

genOutput x = concat $ everyOther $ toList $ fmap (\env@(PdEnv _ states _) -> getData $ index states 6) x
--genOutput x = x

putWave vs =
   let
      riff = 0x46464952
      wave = 0x45564157
      fmts = 0x20746d66
      datx = 0x61746164
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

csh = 554.37
ash = 932.33
g = 783.99
gsh = 830.61
f = 698.46

--
-- osc0.pd
patch = PdPatch 10 (fromList [
            PdAtomBox    [PdFloat 0] [PdControlInlet True "bang"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "osc~", PdFloat gsh] [PdControlInlet True "float", PdControlInlet True "float"] [PdSignalOutlet],
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdFloat 0.5), PdTAtom (PdFloat 1000)]] [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdFloat 0), PdTAtom (PdFloat 100)]] [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdObject     [PdSymbol "line~"] [PdControlInlet True "list", PdControlInlet False "float"] [PdSignalOutlet],
            PdObject     [PdSymbol "*~"] [PdSignalInlet 0, PdSignalInlet 0] [PdSignalOutlet],
            PdObject     [PdSymbol "dac~"] [PdSignalInlet 0] [],
            PdObject     [PdSymbol "r", PdSymbol "metroToggle"] [] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "metro", PdFloat 500] [PdControlInlet True "bang"] [PdControlOutlet "bang"],
            PdObject     [PdSymbol "tabwrite~", PdSymbol "array99"] [PdControlInlet True "signal"] [],
            PdGObject    [PdSymbol "array99"] [] [],
            PdMessageBox [PdCmd (PdReceiver "metroToggle") [PdTAtom (PdFloat 1.0)]] [PdControlInlet True "bang"] [PdControlOutlet "list"],
            PdMessageBox [PdCmd (PdReceiver "metroToggle") [PdTAtom (PdFloat 0.0)]] [PdControlInlet True "bang"] [PdControlOutlet "list"]
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

main :: IO ()
main = 
   ByteString.putStr $ runPut (putWave output)
   where
      output = genOutput $ run 10000 patch [
                        (PdEvent 5 11 Nothing), -- metroToggle 1
                        (PdEvent 10 2 Nothing),  -- 0.1 1000
                        (PdEvent 900 3 Nothing), -- 0 100
                        (PdEvent 1001 0 (Just $ PdFloat csh)),
                        (PdEvent 1002 2 Nothing),  -- 0.1 1000

                        (PdEvent 1900 3 Nothing), -- 0 100
                        (PdEvent 2001 0 (Just $ PdFloat g)),
                        (PdEvent 2002 2 Nothing),  -- 0.1 1000

                        (PdEvent 3660 3 Nothing), -- 0 100
                        (PdEvent 3749 2 Nothing),  -- 0.1 1000
                        
                        (PdEvent 3750 0 (Just $ PdFloat gsh)),
                        (PdEvent 3875 0 (Just $ PdFloat ash)),
                        (PdEvent 4000 0 (Just $ PdFloat gsh)),

                        (PdEvent 4333 0 (Just $ PdFloat f)),

                        (PdEvent 4666 0 (Just $ PdFloat csh)),

                        (PdEvent 5000 0 (Just $ PdFloat g)),

                        (PdEvent 5650 3 Nothing), -- 0 100
                        (PdEvent 5745 2 Nothing),  -- 0.1 1000

                        (PdEvent 5750 0 (Just $ PdFloat gsh)),
                        (PdEvent 5875 0 (Just $ PdFloat ash)),
                        (PdEvent 6000 0 (Just $ PdFloat gsh)),

                        (PdEvent 7000 3 Nothing), -- 0 100

                        (PdEvent 4500 12 Nothing) -- metroToggle 0
                        ]
--}


{--
-- messages.pd
patch = PdPatch 10 (fromList [
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 1), PdTAtom (PdFloat 2)], PdCmd PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdFloat 10), PdTAtom (PdFloat 20)]] [PdControlInlet True "list"] [], 
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdSymbol "list"), PdTAtom (PdSymbol "foo"), PdTAtom (PdFloat 5), PdTAtom (PdFloat 6)]] [PdControlInlet True "list"] [], 
            PdMessageBox [PdCmd PdToOutlet [PdTDollar 1, PdTDollar 1], PdCmd (PdRDollar 1) [PdTDollar 2], PdCmd (PdReceiver "bar") [PdTDollar 2, PdTDollar 1]] [PdControlInlet True "list"] [], 
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
         ]) []

main :: IO ()
main = print (run 30 patch [(PdEvent 1 0 Nothing), (PdEvent 3 1 Nothing)])
--}

{-- inc.pd
patch :: PdPatch
patch = PdPatch 10 (fromList [
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdSymbol "bang")]] [PdControlInlet True "list"] [],
            PdObject     [PdSymbol "float"] [PdControlInlet True "float", PdControlInlet False "float"] [PdControlOutlet "float"],
            PdObject     [PdSymbol "print"] [PdControlInlet True "list"] [],
            PdObject     [PdSymbol "+", PdFloat 1] [PdControlInlet True "float", PdControlInlet False "float"] [PdControlOutlet "float"],

            PdObject     [PdSymbol "print"] [PdControlInlet True "list"] [],
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdSymbol "bang")]] [PdControlInlet True "list"] [],
            PdMessageBox [PdCmd PdToOutlet [PdTAtom (PdSymbol "float"), PdTAtom (PdFloat 100)]] [PdControlInlet True "list"] []

         ]) (fromList [
            PdConnection (0, 0) (1, 0),
            PdConnection (1, 0) (2, 0),
            PdConnection (1, 0) (3, 0),
            PdConnection (3, 0) (1, 1),
            PdConnection (3, 0) (4, 0),
            PdConnection (5, 0) (3, 0),
            PdConnection (6, 0) (3, 1)
         ]) []

main :: IO ()
main = print (run 30 patch [
                        (PdEvent 1 0),
                        (PdEvent 3 0),
                        (PdEvent 5 0),
                        (PdEvent 6 5),
                        (PdEvent 7 5),
                        (PdEvent 8 6),
                        (PdEvent 10 5),
                        (PdEvent 12 0),
                        (PdEvent 13 0),
                        (PdEvent 15 0) ])
--}
