module DrawMats where

import Line
import Screen
import Solids
import Transform

type Args = [String]
data DrawMats =
    DrawMats { getScreen :: Screen
             , getTransform :: Transform Double
             , getEdges :: [Vect Double]
             , getTriangles :: [Triangle Double]
             }

emptyDM :: DrawMats
emptyDM = DrawMats { getScreen = emptyScreen blk (499,499)
                   , getTransform = ident
                   , getEdges = []
                   , getTriangles = []
                   }

modScreen :: (Screen -> Screen) -> DrawMats -> DrawMats
modScreen f dm = dm { getScreen = (f $ getScreen dm) }

modTransform :: (Transform Double -> Transform Double) ->
    DrawMats -> DrawMats
modTransform f dm = dm { getTransform = (f $ getTransform dm) }

modEdges :: ([Vect Double] -> [Vect Double]) -> DrawMats -> DrawMats
modEdges f dm = dm { getEdges = (f $ getEdges dm) }

modTriangles :: ([Triangle Double] -> [Triangle Double]) ->
    DrawMats -> DrawMats
modTriangles f dm = dm { getTriangles = (f $ getTriangles dm) }

