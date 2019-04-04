{-# LANGUAGE FlexibleContexts #-}
module Parse where

import Line
import Screen
import qualified Solids as S
import qualified Transform as T

import System.Directory
import System.IO
import System.Environment
import Control.Monad.State
import System.Process
import qualified Data.Map.Strict as M
import qualified Data.List as L

type Args = [String]
data DrawMats =
    DrawMats { getScreen :: Screen
             , getTransform :: T.Transform Double
             , getEdges :: [Vect Double]
             , getTriangles :: [S.Triangle Double]
             }

emptyDM :: DrawMats
emptyDM = DrawMats { getScreen = emptyScreen blk (499,499)
                   , getTransform = T.ident
                   , getEdges = []
                   , getTriangles = []
                   }

noArgs :: (MonadState DrawMats m, MonadIO m) => [(String, m ())]
noArgs = [ ("ident", ident)
         , ("apply", apply)
         , ("display", display)
         , ("clear", clear)
         ]

wArgs :: (MonadState DrawMats m, MonadIO m) => [(String, Args -> m ())]
wArgs = [ ("save", save)
        , ("line", line)
        , ("scale", scale)
        , ("move", move)
        , ("rotate", rote)
        , ("hermite", hermite)
        , ("bezier", bezier)
        , ("circle", circle)
        , ("sphere", sphere)
        , ("torus", torus)
        , ("box", box)
        ]

parse :: (MonadState DrawMats m, MonadIO m) => Args -> [m ()]
parse []  = []
parse [a] =
    case lookup a noArgs of
        Just c  -> [c]
        Nothing -> []
parse (a:b:xs) =
    case lookup a noArgs of
        Just c0 -> c0 : (parse (b:xs))
        Nothing -> 
            case lookup a wArgs of
                Just c1 -> (c1 $ words b) : (parse xs)
                Nothing -> parse (b:xs)

doublePts :: [a] -> [a]
doublePts [] = []
doublePts (x:xs) = x:x:(doublePts xs)

box :: (MonadState DrawMats m) => Args -> m ()
box args = modify $ modTriangles (++tris)
    where [cx, cy, cz, w, h, d] = map read args
          tris = S.box cx cy cz w h d

sphere :: (MonadState DrawMats m) => Args -> m ()
sphere args = modify $ modTriangles (++tris)
    where [cx, cy, cz, r] = map read args
          tris = S.sphere cx cy cz r
    
torus :: (MonadState DrawMats m) => Args -> m ()
torus args = modify $ modTriangles (++tris)
        where [cx, cy, cz, r0, r1] = map read args
              tris = S.torus cx cy cz r0 r1

circle :: (MonadState DrawMats m) => Args -> m ()
circle args = modify $ modEdges (++pts)
    where [cx, cy, cz, r] = map read args
          pts = connectPts $ T.circle cx cy cz r

hermite :: (MonadState DrawMats m) => Args -> m ()
hermite args = modify $ modEdges (++ (connectPts pts))
    where (fX, fY) = T.genHermFxns args
          pts = L.zipWith4 Vect (T.sampleParam 128 fX) (T.sampleParam 128 fY)
                       (repeat 0) (repeat 1)

bezier :: (MonadState DrawMats m) => Args -> m ()
bezier args = modify $ modEdges (++ (connectPts pts))
    where (fX, fY) = T.genBezFxns args
          pts = L.zipWith4 Vect (T.sampleParam 128 fX) (T.sampleParam 128 fY)
                       (repeat 0) (repeat 1)

clean :: (MonadState DrawMats m) => m ()
clean = modify . modScreen $ const (emptyScreen blk (499,499))

draw :: (MonadState DrawMats m, MonadIO m) => m ()
draw = do
    dm <- get
    modify $ modScreen $ (drawEdges red (getEdges dm))
    modify $ modScreen $ (S.drawTriangles red (getTriangles dm))

apply :: (MonadState DrawMats m) => m ()
apply = do
    dm <- get
    modify . modEdges $ T.mmult (getTransform dm)
    modify . modTriangles $ map (S.trTriangle $ getTransform dm)

save :: (MonadState DrawMats m, MonadIO m) => Args -> m ()
save args = do
    let path = head args
    clean
    draw
    dm <- get
    liftIO $ do
        writeFile ".tempimg.ppm" (printPixels $ getScreen dm)
        callProcess "convert" [".tempimg.ppm", path]
        removeFile ".tempimg.ppm"

display :: (MonadState DrawMats m, MonadIO m) => m ()
display = do
    clean
    draw
    dm <- get
    liftIO $ do
        writeFile ".tempimg.ppm" (printPixels $ getScreen dm)
        callProcess "eog" [".tempimg.ppm"]
        removeFile ".tempimg.ppm"
--      (tempName, tempHandle) <- openTempFile "." "disp.ppm"
--      hPutStrLn tempHandle (printPixels (499, 499) scrn)
--      callProcess "eog" [tempName]
--      hClose tempHandle 
--      removeFile tempName
--              god damn why doesn't this work

line :: (MonadState DrawMats m) => Args -> m ()
line args = modify . modEdges $ addLine ln
    where [x0, y0, z0, x1, y1, z1] = map read args
          ln = Line (Vect x0 y0 z0 1) (Vect x1 y1 z1 1)

ident :: (MonadState DrawMats m) => m ()
ident = modify . modTransform $ const T.ident

scale :: (MonadState DrawMats m) => Args -> m ()
scale args = modify . modTransform $ (`mappend` T.scale x y z)
    where [x, y, z] = map read args

rote :: (MonadState DrawMats m) => Args -> m ()
rote s = modify . modTransform $ (`mappend` roti s)
    where roti args
            | axis == "x"   = T.rotX theta
            | axis == "y"   = T.rotY theta
            | axis == "z"   = T.rotZ theta
            where axis  = args !! 0
                  theta = read $ args !! 1

move :: (MonadState DrawMats m) => Args -> m ()
move args = modify . modTransform $ (`mappend` T.trans x y z)
    where [x, y, z] = map read args

clear :: (MonadState DrawMats m) => m ()
clear = modify . modEdges $ const []

modScreen :: (Screen -> Screen) -> DrawMats -> DrawMats
modScreen f dm = dm { getScreen = (f $ getScreen dm) }

modTransform :: (T.Transform Double -> T.Transform Double) ->
    DrawMats -> DrawMats
modTransform f dm = dm { getTransform = (f $ getTransform dm) }

modEdges :: ([Vect Double] -> [Vect Double]) -> DrawMats -> DrawMats
modEdges f dm = dm { getEdges = (f $ getEdges dm) }

modTriangles :: ([S.Triangle Double] -> [S.Triangle Double]) ->
    DrawMats -> DrawMats
modTriangles f dm = dm { getTriangles = (f $ getTriangles dm) }
