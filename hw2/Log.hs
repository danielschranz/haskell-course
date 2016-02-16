-- CIS 194 Homework 2

module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file



-------------------------------- ex 1
parseMessage :: String -> LogMessage
parseMessage message = parseHelper (words message)
  where parseHelper :: [String] -> LogMessage
        parseHelper ("E":et:ts:s) = LogMessage (Error (read et ::Int)) (read ts ::TimeStamp) (unwords s)
        parseHelper ("I":ts:s) = LogMessage Info (read ts ::TimeStamp) (unwords s)
        parseHelper ("W":ts:s) = LogMessage Warning (read ts ::TimeStamp) (unwords s)
        parseHelper s = Unknown (unwords s)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-------------------------------- ex 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ messageTs _) (Node low root@(LogMessage _ rootTs _) high)
 | rootTs < messageTs = Node low root (insert message high)
 | otherwise = Node (insert message low) root high

------------------------------- ex 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

------------------------------- ex 4
inorder :: MessageTree -> [LogMessage]
inorder Leaf = []
inorder (Node left root right) = inorder left ++ [root] ++ inorder right

------------------------------- ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getString . inorder . build . filter severeErrors
  where severeErrors :: LogMessage -> Bool
        severeErrors (LogMessage (Error n) _ s) = n >= 50
        severeErrors _ = False

        getString :: LogMessage -> String
        getString (LogMessage _ _ s) = s
