import Control.Exception
import Control.Monad.State
-- import Data.List.Split
import System.Console.ANSI  -- http://hackage.haskell.org/package/ansi-terminal-0.5.0/docs/System-Console-ANSI.html#2
import System.IO
import System.IO.Error

-- -----------------------------------------------------------------------------
-- Constants 
-- -----------------------------------------------------------------------------
-- intro text
intro = "[ESC+o] Open file\t" ++  -- Note: We do not use CTRL+, as those key combinations are appearently caught by the terminal
        "[ESC+s] Save file\t" ++
        "[ESC+x] Exit\t"
        
type EditorState = (Int, [Char]) -- (Cursor pos, text)
        

-- insert a character into the text at the current cursor position. Cursor position advanced by 1
insert :: Char -> State EditorState () 
insert c = state $ \(p,xs) -> ((), (p+1 , let (ys,zs) = splitAt p xs in ys++c:zs))

foo :: State EditorState ()
foo = do  
    put (2, "ABC")
    
-- runState foo (0,[])
-- runState foo ['a','b','c','d']

-- execState foo (0,[]) -- return result
-- evalState foo (0, []) -- return final state

-- evalState :: State s a -> s -> a
-- evalState act = fst . runState act
 
-- execState :: State s a -> s -> s
-- execState act = snd . runState act
    
-- -----------------------------------------------------------------------------
-- State + IO example
-- http://www.haskell.org/haskellwiki/Simple_StateT_use
-- -----------------------------------------------------------------------------
main :: IO ()
main = runStateT code [1..] >> return ()

-- layer an infinite list of uniques over the IO monad
code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop 
    io $ print y
    return ()
    
-- pop the next unique off the stack
pop :: StateT [Integer] IO Integer
pop = do 
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO
    
-- -----------------------------------------------------------------------------
-- main
-- -----------------------------------------------------------------------------
-- main = do 
--     printIntro
    -- configure streams
--     hSetBuffering stdin NoBuffering 
--     hSetBuffering stdout NoBuffering
--     hSetEcho stdin False    
--     display (runState foo (0,[]))
--     forever $ do
--         readNext >>= processInput
 
-- -----------------------------------------------------------------------------
-- Input processing
-- -----------------------------------------------------------------------------
-- read the next character from keyboard
readNext :: IO Char
readNext = getChar
    
-- Clear screen and print some nice intro text
printIntro :: IO ()
printIntro = do 
    clearScreen
--     putStr moveTo11 --move cursor to position (1,1)
    putStrLn intro

processInput :: Char -> IO ()
processInput '\x1b' = readNext >>= esc     -- matched escape character; read (next part of) escape character code and process it
processInput c = 
--     insert c 
    putChar c         -- any other character is simply printed to stdout
    
-- Assumes the character read before c was ESC and thus processes the following characters as escape codes.
esc :: Char -> IO ()
esc '[' = readNext >>= escSqBracket
esc 'o' = savelyOpen
esc 's' = savelyWrite
esc 'x' = do
    resetScreen "New file"
    editMode    
esc c = putStrLn $ "Unexpected escape char: " ++ (show c)
    
-- Assumes that the characters read before c were ESC[ and processes the following character as part of the arrow key scancode.
escSqBracket :: Char -> IO ()
escSqBracket 'A' = cursorUpLine 1
escSqBracket 'B' = cursorDownLine 1
escSqBracket 'C' = cursorForward 1
escSqBracket 'D' = cursorBackward 1
escSqBracket c = putStrLn $ "Unexpected escape char: " ++ (show c)
    

-- -----------------------------------------------------------------------------
-- Modes:
-- Edit mode: catch all keystrokes and process them
-- Default mode: Let the terminal handle keystrokes (i.e., print the in the terminal)
-- -----------------------------------------------------------------------------
editMode = hSetEcho stdin False
defaultMode = hSetEcho stdin True -- TODO: special characters like backspace not processed correctly

    
-- -----------------------------------------------------------------------------
-- Screen manipulation
-- -----------------------------------------------------------------------------
display :: EditorState -> IO ()
display (p, xs) = do
    clearScreen
    putStr xs
    setCursorPosition 0 p

resetScreen :: String -> IO ()
resetScreen s = do
    clearScreen
    setCursorPosition 0 0
    setTitle s
    
-- -----------------------------------------------------------------------------
-- File IO
-- -----------------------------------------------------------------------------
savelyOpen :: IO ()  
savelyOpen = do
    resetScreen "New file"
    defaultMode
    putStr "Tell me which file you want to edit (CTRL-C to quit): "
    rf <- getLine
    result <- try (readFile rf) :: IO (Either SomeException String)
    case result of
        Left ex  -> do
            putStrLn $ "Whops, something went wrong: " ++ show ex
            savelyOpen
        Right content -> do
            resetScreen rf
            editMode
            putStrLn $ content 
                            
savelyWrite:: IO ()  
savelyWrite = do 
    resetScreen "Saving file" -- TODO: Saving files needs to be handled better
    defaultMode
    putStr "Tell me where to save: "
    wf <- getLine
    result <- try (writeFile wf "foo") :: IO (Either SomeException ())
    case result of
        Left ex  -> do
            putStrLn $ "Whops, something went wrong: " ++ show ex
            savelyWrite
        Right res -> do
            resetScreen "New file"
            editMode


        -- putStrLn $ "\x1b[31m" ++ "Make me red" ++ "\x1b[0m" ++ " but leave me black" --color text