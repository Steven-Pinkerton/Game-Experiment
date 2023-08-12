module Main where

initializeWindow :: IO Window
initializeWindow = do
  _ <- GLFW.init
  GLFW.createWindow 800 600 "Your Game Title" Nothing Nothing

initializeWorld :: IO World
initializeWorld = do
  w <- initWorld
  -- Add entities and associate components as needed
  return w

mainLoop :: Window -> World -> IO ()
mainLoop window world = do
  -- Check if window should close (e.g. if the 'x' button was pressed)
  shouldClose <- GLFW.windowShouldClose window
  unless shouldClose $ do
    -- Clear screen or frame buffer
    GL.clear [GL.ColorBuffer]

    -- Update the game state (e.g., positions, physics, animations, etc.)
    updateGameState world

    -- Render your scene
    render world

    -- Swap front and back buffers
    GLFW.swapBuffers window

    -- Poll for and process events
    GLFW.pollEvents

    -- Run garbage collector
    performGC -- This comes from the System.Mem module

    -- Recur for next frame
    mainLoop window world


main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    putTextLn "Hello 🌎"
