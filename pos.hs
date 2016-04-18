import Data.Time.Clock
import Data.Time.Calendar

taxRate = 0.065

digitize :: Integer -> [Integer]
digitize 0 = []
digitize x = digitize (div x 10) ++ [mod x 10]
           
cardType :: Integer -> String
cardType n | head (digitize n) == 4 = "Visa"
           | head (digitize n) == 5 = "MasterCard"
           | head (digitize n) == 6 = "Discover"
           | otherwise = "Not Accepted"

pastDate :: Integer -> Int -> Int -> IO Bool
pastDate y m d = do
    currDate <- fmap utctDay getCurrentTime
    return ((fromGregorian y m d) < currDate)

calcTax :: Float -> Float
calcTax x = x * taxRate

verifyCard :: Integer -> Integer -> Int -> Int -> IO Bool
verifyCard cardNum year month day = do
    expired <- pastDate year month day
    return (((cardType cardNum) /= "Not Accepted") && not(expired))

checkOut :: Float -> IO ()
checkOut num = do
    putStrLn "Enter cost: "
    cost <- getLine
    let amount = (read cost :: Float)
    if amount /= 0
        then do putStrLn (show amount)
                checkOut (amount + num)
    else putStrLn (show (amount + (num + (calcTax (amount + num)))))
