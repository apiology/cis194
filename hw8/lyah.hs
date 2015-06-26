reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main2 :: IO ()
main2 = do
  line <- getLine
  if null line
    then return ()
    else do
        putStrLn $ reverseWords line
        main


main :: IO ()
main = getLine >>= (\line -> if null line
                             then
                               return ()
                             else
                               putStrLn (reverseWords line) >> main)

