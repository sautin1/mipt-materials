import System.Random

type State num          =   [num]
type StateBounds num    =   [(num, num)]
data StateData num      =   StateData {
                                stGen       :: StdGen,
                                stCur       :: State num
                            }
type StateF num         =   StateData num -> StateData num
data StateInfo num      =   StateInfo {
                                stNext      :: StateF num,
                                stBounds    :: StateBounds num,
                                stStart     :: State num
                            }

type Temperature        =   Double
type TemperatureBounds  =   (Temperature, Temperature)
type TemperatureF       =   Temperature -> Int -> Temperature
data TemperatureInfo    =   TemperatureInfo {
                                tempNext    :: TemperatureF,
                                tempBounds  :: TemperatureBounds
                            }

type EnergyF num = State num -> Temperature

annealMin   :: Num num  => EnergyF num
                        -> StateInfo num
                        -> TemperatureInfo
                        -> IO (State num)
annealMin energyF stateInfo tempInfo = do
    gen <- newStdGen
    let randomNums = randomRs (0.0, 1.0) gen :: [Double]
    stateGen <- getStdGen
    let stateData = StateData {
                        stCur = stStart stateInfo,
                        stGen = stateGen
                    }
    return $ annealMinIter randomNums stateData 1 tMax
        where
            tMax = snd $ tempBounds tempInfo
            annealMinIter (rand:rands) stData iterN tCur
                | tCur > tMin   = annealMinIter rands (nextState stData) (iterN + 1) $ (tempNext tempInfo) tCur iterN
                | otherwise     = stCur stData
                    where
                        tMin = fst $ tempBounds tempInfo
                        nextState sData
                            | rand <= transStProb   =   transStData
                            | otherwise             =   StateData {
                                                            stCur = stCur sData,
                                                            stGen = stGen transStData
                                                        }
                                where
                                    transStData = (stNext stateInfo) sData
                                    deltaEn     = (energyF $ stCur transStData) - (energyF $ stCur sData)
                                    transStProb = exp $ negate $ deltaEn / tCur

annealMax   :: Num num  => EnergyF num
                        -> StateInfo num
                        -> TemperatureInfo
                        -> IO (State num)
annealMax energyF = annealMin (negate . energyF)

main = do
    --result <- annealMin energyExample tempLinear stateTransExample (0.0001, 10) (-1)
    --print result
    putStrLn "Lol"