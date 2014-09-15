import Control.Exception
import Control.Monad.State
-- import Data.List.Split
import System.Console.ANSI  -- http://hackage.haskell.org/package/ansi-terminal-0.5.0/docs/System-Console-ANSI.html#2
import System.IO
import System.IO.Error

-- -----------------------------------------------------------------------------
-- Constants 
-- -----------------------------------------------------------------------------
type EditorState = (Int, [Char]) -- (Cursor pos, text)
        
-- initial state
initalState = (0,[])

-- intro text
intro = "[ESC+o] Open file\t" ++  -- Note: We do not use CTRL+, as those key combinations are appearently caught by the terminal
        "[ESC+s] Save file\t" ++
        "[ESC+x] Exit\t"
        
-- -----------------------------------------------------------------------------
-- main
-- -----------------------------------------------------------------------------
main :: IO ()
main = do 
    -- configure streams
    hSetBuffering stdin NoBuffering 
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False    
    execStateT process initalState     -- run the editor
    putStrLn "bye"              -- this is only here because the last statement needs to be an expression with result type IO

-- -----------------------------------------------------------------------------
-- Handling the editors state
-- -----------------------------------------------------------------------------
process :: StateT EditorState IO ()
process = do
    (lift readNext) >>= processInput  -- wait for next keystroke and process it
    (p,text) <- get                       -- get current state of our editor  
    lift clearScreen                  
    lift $ setCursorPosition 0 0
    lift $ putStr text
    process

-- insert a character into the text at the current cursor position. Cursor position advanced by 1
insert :: Char -> StateT EditorState IO ()
insert c = state $ \(p,xs) -> ((), (p+1 , let (ys,zs) = splitAt p xs in ys++c:zs))
                                              
-- -----------------------------------------------------------------------------
-- Input processing
-- -----------------------------------------------------------------------------
-- read the next character from keyboard
readNext :: IO Char
readNext = getChar
    
processInput :: Char -> StateT EditorState IO ()
processInput '\x1b' = (lift readNext) >>= esc  -- matched escape character; read (next part of) escape character code and process it
processInput c =  insert c  -- any other character is simply added to our text buffer
    
-- Assumes the character read before c was ESC and thus processes the following characters as escape codes.
esc :: Char -> StateT EditorState IO ()
esc '[' = (lift readNext) >>= escSqBracket 
esc 'o' = lift savelyOpen
esc 's' = lift savelyWrite
-- esc 'x' = do                         -- TODO
--     resetScreen "New file"
--     editMode    
esc c = lift $ putStrLn ("Unexpected escape char: " ++ (show c))
    
-- Assumes that the characters read before c were ESC[ and processes the following character as part of the arrow key scancode.
escSqBracket :: Char -> StateT EditorState IO ()
escSqBracket 'A' = lift $ cursorUpLine 1
escSqBracket 'B' = lift $ cursorDownLine 1
escSqBracket 'C' = lift $ cursorForward 1
escSqBracket 'D' = lift $ cursorBackward 1
escSqBracket c = lift $ putStrLn ("Unexpected escape char: " ++ (show c))
    

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
-- Clear screen and print some nice intro text
printIntro :: IO ()
printIntro = do 
    clearScreen
--     putStr moveTo11 --move cursor to position (1,1)
    putStrLn intro
    
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