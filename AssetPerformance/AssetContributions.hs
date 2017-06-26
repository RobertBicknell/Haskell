import qualified  Data.List as DL

-- sample time series prices.
assetA = [100.0,110.0,105.0,102.0,117.0,120.0,110.0,106.0,120.0,125.0, 125.0, 125.0, 125.0]
assetB = [100.0,100.0,105.0,100.0,99.0,102.0,110.0,104.0,112.0,115.0, 115.0, 117.0, 110.0]
assetC = [50.0, 55.0, 56.0, 61.0, 55.0, 57.0,59.0,60.0,57.0,57.0, 58.0, 59.0, 60.0]

s2 = [(assetA, wA), (assetB, wB), (assetC, wC)] --note - weights not used.

priorX x = head(x) : x
returnsX a = zipWith (\x y -> ((x/y)-1) ) a (priorX a)
--f (x,y) =  zipWith (*) x y
f (x,y) = x --ignore weights (units of asset) for now....

assetDailyValues = map ( f  ) s2
portDailyValues = map sum (DL.transpose assetDailyValues)
assetDailyWeight x = zipWith (/) x portDailyValues 
assetWeightedReturns x  = zipWith (*) (returnsX x) (assetDailyWeight x)
dailyPortfolioPerfs a = zipWith (\x y -> ((x/y)-1) )  a (priorX a)
weightedReturnsWithPortfolioPerfs x = zip (assetWeightedReturns x) (dailyPortfolioPerfs portDailyValues)
assetCumWeightedReturns x  = tail $ scanl (\acc x -> (1 + (snd x) )*(acc) + (fst x) ) 0 (weightedReturnsWithPortfolioPerfs x)
allAssetsCumWeightedReturns = map assetCumWeightedReturns assetDailyValues

-----------------------------------------------------------------------
main = do
    putStrLn $ show allAssetsCumWeightedReturns

-----------------------------------------------------------------------

--todo

-- add tests
--	sum asset contributions - must equal portfolio cumulative performance

-- implement weights
-- use of variable weights (i.e. due to trading and portfolio transfers) 
-- requires further corrections to daily performance to prevent disorted performance. 
wA :: [Float]
wA = [1.0,1.1]
wB = [1.0,1.2]
wC = [1.1,0.9]













