module DrawMats where

import Line
import Screen
import Solids
import Transform

type Args = [String]
data DrawMats =
     DrawMats { getScreen :: Screen
              , getTStack :: [Transform Double]
--            , getEdges :: [Vect Double]
--            , getTriangles :: [Triangle Double]
              }

emptyDM :: DrawMats
emptyDM = DrawMats { getScreen = emptyScreen blk (499,499)
                   , getTStack = [ident]
--                 , getEdges = []
--                 , getTriangles = []
                   }

modScreen :: (Screen -> Screen) -> DrawMats -> DrawMats
modScreen f dm = dm { getScreen = f $ getScreen dm }

getTransform :: DrawMats -> Transform Double
getTransform = head . getTStack

popTransform :: DrawMats -> DrawMats
popTransform dm = dm { getTStack = tail $ getTStack dm }

pushTransform :: DrawMats -> DrawMats
pushTransform dm = dm { getTStack = (tf:tf:tfs) }
    where (tf:tfs) = getTStack dm

modTransform :: (Transform Double -> Transform Double) -> DrawMats -> DrawMats
modTransform f dm = dm { getTStack = ((f tf):tfs) }
    where (tf:tfs) = getTStack dm

