data Answer = I Int Integer | Io Int (IO Integer) | Ios Int (IO String)

showAnswer :: Answer -> IO String
showAnswer (I   p ans) = pure $ "Problem " ++ show p ++ "\t" ++ show ans
showAnswer (Io  p ans) = (("Problem " ++ show p ++ "\t") ++) . show <$> ans
showAnswer (Ios p ans) = (("Problem " ++ show p ++ "\t") ++) <$> ans

set :: Int -> Either Integer (Either (IO Integer) (IO String)) -> Answer
set idx (Left  ans        ) = I idx ans
set idx (Right (Left  ans)) = Io idx ans
set idx (Right (Right ans)) = Ios idx ans

counts :: Int
counts = length answers

answers :: [Answer]
answers = zipWith
    set
    [1 ..]
