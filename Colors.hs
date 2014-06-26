module Colors (RGBColor (RGBTuple, RGBString),
               toggleRGBdescription) where


data RGBColor = RGBTuple (Float, Float, Float)
              | RGBString String deriving Show


toggleRGBdescription :: RGBColor -> RGBColor

toggleRGBdescription (RGBTuple (x, y, z)) =
    RGBString $ "#" ++ to2digitHex x
                    ++ to2digitHex y
                    ++ to2digitHex z

toggleRGBdescription (RGBString s) =
    let xh = [(s !! 1), (s !! 2)]
        yh = [(s !! 3), (s !! 4)]
        zh = [(s !! 5), (s !! 6)]
    in  RGBTuple (from2digitHex xh,
                  from2digitHex yh,
                  from2digitHex zh)


hexDigits :: Int -> Char
hexDigits = (!!) "0123456789abcdef"


decimalDigits :: Char -> Int
decimalDigits c = case c of
                      '0' -> 0
                      '1' -> 1
                      '2' -> 2
                      '3' -> 3
                      '4' -> 4
                      '5' -> 5
                      '6' -> 6
                      '7' -> 7
                      '8' -> 8
                      '9' -> 9
                      'a' -> 10
                      'b' -> 11
                      'c' -> 12
                      'd' -> 13
                      'e' -> 14
                      'f' -> 15
                      _ -> 0

-- the following function are unsafe and an implementation detail, nobody
-- should see!

-- | string must have at least 2 character and should not have more than 2.
from2digitHex :: String -> Float
from2digitHex s =

    let digit1 = decimalDigits $ s !! 0
        digit2 = decimalDigits $ s !! 1

    in  (fromIntegral digit1 * 16 + fromIntegral digit2) / 255


-- | accept only a number in [0, 1]
to2digitHex :: Float -> String
to2digitHex n =

    let byte = round $ 255 * n::Int
        digit1 = hexDigits (byte `div` 16)
        digit2 = hexDigits (byte `mod` 16)

    in  [digit1, digit2]
