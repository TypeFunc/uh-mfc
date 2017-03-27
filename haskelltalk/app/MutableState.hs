
module MutableState where

import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import Data.STRef (newSTRef, readSTRef, modifySTRef, writeSTRef)
import Control.Concurrent (newMVar, newEmptyMVar, readMVar, modifyMVar, putMVar, takeMVar, forkIO, threadDelay)
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)


--IORef is the most basic mutable reference.  There is also STRef, MVar, and TVar for
--software transactional memory. All have similar functions as you can see above.
ref = newIORef "bbq"

writeit ref = do
  theref <- ref
  writeIORef theref "lol"
  readIORef theref >>= putStrLn
  return theref
  
printit ref = do
  theref <- ref
  readIORef theref >>= putStrLn
  return theref

bbqlollol = printit $ writeit $ printit ref

--IORef bubblesort. Note return type is IO [b]
bubbleSort :: Ord b => [b] -> IO [b]
bubbleSort input = do
    let ln = length input

    xs <- mapM newIORef input

    forM_ [0..ln - 1] $ \_ -> do
        forM_ [0..ln - 2] $ \j -> do
            let ix = xs !! j
            let iy = xs !! (j + 1)

            x <- readIORef ix
            y <- readIORef iy

            when (x > y) $ do
                writeIORef ix y
                writeIORef iy x

    mapM readIORef xs

--STRefs. Note how we can use runST to "escape" the ST monad, so the type is Int -> Int
increment :: Int -> Int
increment x = runST $ do
    ref <- newSTRef 0
    modifySTRef ref (+x)
    readSTRef ref

-- STRef bubblesort. Note return type is just [b], i.e.
-- We can hide mutable state within a pure function!
bubbleSort' :: Ord b => [b] -> [b]
bubbleSort' input = runST $ do
    let ln = length input

    xs <- mapM newSTRef input

    forM_ [0..ln - 1] $ \_ -> do
        forM_ [0..ln - 2] $ \j -> do
            let ix = xs !! j
            let iy = xs !! (j + 1)

            x <- readSTRef ix
            y <- readSTRef iy

            when (x > y) $ do
                writeSTRef ix y
                writeSTRef iy x

    mapM readSTRef xs


{- MVars.  While an IORef must always have a value, MVar can be empty.
If we try to do takeMVar from an empty MVar, it will block the thread until someone else puts a value into the MVar. The same thing happens when you try to putMVar into an MVar that already has a value, it will block until someone takes that value out. -}

mvar = do
      a <- newEmptyMVar
      putMVar a "hello"
      takeMVar a >>= putStrLn

fork1 = do
    forkIO $ do
        threadDelay 2000000
        putStrLn "Hello World"
    -- evaluation in the main thread will continue and the runtime will kill the other thread before it can print "HelloWorld"
    putStrLn "Game over!"

fork2 = do
    a <- newEmptyMVar

    forkIO $ do
        threadDelay 2000000
        putStrLn "Hello World"
        putMVar a ()

    takeMVar a -- evaluation in the main thread will pause for 2 seconds while takeMVar waits for the other thread to putMVar.  Both strings are printed.
    putStrLn "Game over!"

--TODO Software Transactional Memory with TVars and atomically
