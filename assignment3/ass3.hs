import Control.Exception
import Control.Monad.State
import Data.List
-- import Data.List.Split
import System.Console.ANSI  -- http://hackage.haskell.org/package/ansi-terminal-0.5.0/docs/System-Console-ANSI.html#2
import System.IO
import System.IO.Error

-- -----------------------------------------------------------------------------
-- Constants 
-- -----------------------------------------------------------------------------
type Text = [Char]
type EditorState = (Int, Text) -- (Cursor pos, text)
        
-- initial state
initalState = (0,[])

-- Size of the xterm window our editor runs in. Assumed to be constant (i.e., don't change the window size of the xterm or things will get really weird)
nrCols = 150
nrRows = 50

-- intro text
intro = "[ESC+o] Open file\t" ++  -- Note: We do not use CTRL+, as those key combinations are appearently caught by the terminal
        "[ESC+s] Save file\t" ++
        "[ESC+x] Exit\t"

tt = "AB\nC\nDEFG\n\nHIJKLMNOP\n"        
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
    (lift readNext) >>= processInput        -- wait for next keystroke and process it
    (p,text) <- get                         -- get current state of our editor  
    lift clearScreen                  
    lift $ setCursorPosition 0 0            -- set cursor to 0,0 so that...
    lift $ putStr (highlight text)          -- ...text is printed correctly
    let cp = cursorPosition text p in do    -- calculate cursor position
        lift $ putStrLn ("  " ++ (show p ) ++ "->" ++ show (fst cp) ++ ":" ++ show (snd cp) ++ "   ") -- TODO: remove debug code
        lift $ setCursorPosition (fst cp) (snd cp)  -- set actual cursor position
    process

-- insert a character into the text at the current cursor position. Cursor position advanced by 1
insertChar :: Char -> StateT EditorState IO ()
insertChar c = state $ \(p,xs) -> ((), (p+1 , insertAt c xs p))
   
insertAt :: Char -> Text -> Int -> Text
insertAt c xs p = let (ys,zs) = splitAt p xs in ys++c:zs
 
-- -----------------------------------------------------------------------------
-- Calculating cursor position on screen
-- Arguments:   String text: contains the text to display, including \n;
--              Int p: cursor position within that text
-- Return:      Int cp=(x,y): cursor position on screen
-- Algorithm:
--      IF cursor is at very end of text AND last character is \n
--          THEN RETURN (nr of lines + nr wrapped lines, 0)
--          ELSE    l1 <- Split the text into lines
--                  l2 <- length of each line + 1 (we assume that each line ends with \n)
--                  l3 <- prefix sum over l2
--                  line <- s.t. l3[line] > p (find the line in which text[p] will be displayed)
--                  line <- line + nr lines with length > nrCols up until position p in the text
--                  column <- p - nr of characters in all lines before line, mod nr of columns
--                  RETURN (line, column)
-- -----------------------------------------------------------------------------
cursorPosition :: Text -> Int -> (Int, Int)
cursorPosition [] _ = (0,0)
cursorPosition text p =     
    if (p == (length text)) && (text!!(p-1) == '\n')
       then (length (lines text) + (nrLineWraps text p) , 0) -- If the last character is a newline, the else branch will give a wrong line number (because we add +1 to the length of every line)
       else (lineNr + (nrLineWraps text p),  mod colNr nrCols )               
            where   sl = scanl (+) 0 $ map ((+1) . length) (lines text) -- find out how many characters there are in each line (+1 for newlines) and do prefix sum
                    lineNr = case findIndex (>p) sl of  -- find out in which line we are at
                        Just val -> val - 1
                        Nothing -> -1          --TODO: this can never happen
                    colNr = mod (p - sl!!(lineNr)) nrCols  -- substract nr of characters in all lines before lineNr and do mod nrCols since we wrap long lines)

-- Count number of line wraps that occur in the text up until the given position                 
-- In other words: find lines longer than nrCols (up to given position p in text) and then calculate how often they will wrap
nrLineWraps :: Text -> Int -> Int
nrLineWraps text p = sum wrapCnt
    where 
        t = lines (take p text)                 -- lines up until position p
        ll = filter ((>=nrCols) . length) t     -- long lines 
        lll = map length ll                     -- length of the long lines
        wrapCnt = map (\x -> div x nrCols) lll           -- how often they will wrap

-- -----------------------------------------------------------------------------
-- Changing cursor position
-- -----------------------------------------------------------------------------
setCursor :: Int -> StateT EditorState IO ()
setCursor x = state $ \(p,xs) -> ((), (x, xs))

cForward :: StateT EditorState IO ()
cForward = state $ \(p,xs) -> ((), (min (p+1) (length xs), xs))

cBackward :: StateT EditorState IO ()
cBackward = state $ \(p,xs) -> ((), (max (p-1) 0, xs))
       
-- -----------------------------------------------------------------------------
-- Syntax highlighting
-- -----------------------------------------------------------------------------
highlight:: String -> String
highlight s = s  -- TODO: This is a dummy implementation. Highlight portions of text by inserting color codes like this:
-- "\x1b[31m" ++ "Make me red" ++ "\x1b[0m" ++ " but leave me black" --color text
-- Here be syntax highlighting magic
       
-- -----------------------------------------------------------------------------
-- Input processing
-- -----------------------------------------------------------------------------
-- read the next character from keyboard
readNext :: IO Char
readNext = getChar
    
processInput :: Char -> StateT EditorState IO ()
processInput '\x1b' = (lift readNext) >>= esc  -- matched escape character; read (next part of) escape character code and process it
-- processInput '\n' = lift $ putStrLn "\n"
-- processInput '\r' = lift $ putStrLn "\r"
processInput c =  insertChar c  -- any other character is simply added to our text buffer
    
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
escSqBracket 'A' = lift $ cursorUpLine 1 -- TODO
escSqBracket 'B' = lift $ cursorDownLine 1 -- TODO
escSqBracket 'C' = cForward 
escSqBracket 'D' = cBackward 
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