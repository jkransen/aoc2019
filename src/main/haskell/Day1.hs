import System.Environment

fuelRequired mass = (mass `div` 3) - 2

recurseFuel mass cumulative = do
  let fuelReq = fuelRequired mass
  if (fuelReq > 0)
      then recurseFuel fuelReq (cumulative + fuelReq)
      else cumulative

netFuelRequired mass = recurseFuel mass 0

main :: IO ()
main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFiles = lines content
   let masses = map (read::String->Int) linesOfFiles
   let grosses = map fuelRequired masses
   let nets = map netFuelRequired masses 
   let gross = sum grosses
   let net = sum nets
   putStrLn $ show gross
   putStrLn $ show net



