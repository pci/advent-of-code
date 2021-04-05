module Lib
    ( someFunc
    ) where

import Text.Regex.Base
import Text.Regex.PCRE((=~)) -- or DFA or PCRE or PosixRE

someFunc = length . filter validatePassport2 . map lineToUncheckedPassport . splitStringoOnePassportPerLine

-- Remove blank lines (passport seperaters) and join all passport details onto one line
splitStringoOnePassportPerLine :: [String] -> [String]
splitStringoOnePassportPerLine = (map $ join " ") . split ""

join :: String -> [String] -> String
join sep [] = ""
join sep [x] = x
join sep (x:xs) = x ++ sep ++ (join sep xs)

split :: Eq a => a -> [a] -> [[a]]
split sep [] = []
split sep (x:xs)
    | x == sep = split sep xs
    | otherwise = (takeWhile (/= sep) (x:xs)):(split sep $ dropWhile (/= sep) (x:xs))

data Passport = Passport {
    byr :: String, iyr :: String, eyr :: String, hgt :: String, hcl :: String, ecl :: String, pid :: String, cid :: String
}
  deriving (Eq, Show)

keyVal :: String -> (String, String)
keyVal = (\x -> (x!!0, x!!1)) . split ':'

lineToUncheckedPassport :: String -> Passport
-- use "" as invalid
lineToUncheckedPassport = createPassport Passport { byr = "", iyr = "", eyr = "", hgt = "", hcl = "", ecl = "", pid = "", cid = "" } . map keyVal . split ' '

createPassport :: Passport -> [(String, String)] -> Passport
createPassport p [] = p
createPassport p ((k,v):xs)
    -- urgh strong typing...
    | k == "byr" = createPassport p {byr = v} xs
    | k == "iyr" = createPassport p {iyr = v} xs
    | k == "eyr" = createPassport p {eyr = v} xs
    | k == "hgt" = createPassport p {hgt = v} xs
    | k == "hcl" = createPassport p {hcl = v} xs
    | k == "ecl" = createPassport p {ecl = v} xs
    | k == "pid" = createPassport p {pid = v} xs
    | k == "cid" = createPassport p {cid = v} xs
    | otherwise = createPassport p xs

validatePassport1 :: Passport -> Bool
validatePassport1 p
    | byr p == "" = False
    | iyr p == "" = False
    | eyr p == "" = False
    | hgt p == "" = False
    | hcl p == "" = False
    | ecl p == "" = False
    | pid p == "" = False
    -- cid is optional:
    -- | cid p == "" = False
    | otherwise = True


validatePassport2 :: Passport -> Bool
validatePassport2 p = checkbyr p && checkiyr p && checkeyr p && checkhgt p && checkhcl p && checkecl p && checkpid p

checkbyr :: Passport -> Bool
checkbyr s
  | length res == 0 = False
  | otherwise = 1920 <= (read $ res!!0!!1) && (read $ res!!0!!1) <= 2002
  where
    res = byr s =~ "^(\\d{4})$" :: [[String]]

checkiyr :: Passport -> Bool
checkiyr s
  | length res == 0 = False
  | otherwise = 2010 <= (read $ res!!0!!1) && (read $ res!!0!!1) <= 2020
  where
    res = iyr s =~ "^(\\d{4})$" :: [[String]]

checkeyr :: Passport -> Bool
checkeyr s
  | length res == 0 = False
  | otherwise = 2020 <= (read $ res!!0!!1) && (read $ res!!0!!1) <= 2030
  where
    res = eyr s =~ "^(\\d{4})$" :: [[String]]

checkhgt :: Passport -> Bool
checkhgt s
  | length res == 0 = False
  | res!!0!!2 == "cm" = 150 <= (read $ res!!0!!1) && (read $ res!!0!!1) <= 193
  | res!!0!!2 == "in" = 59 <= (read $ res!!0!!1) && (read $ res!!0!!1) <= 76
  | otherwise = False
  where
    res = hgt s =~ "^(\\d+)(cm|in)$" :: [[String]]

checkhcl :: Passport -> Bool
checkhcl s
  | length res == 0 = False
  | otherwise = True
  where
    res = hcl s =~ "^#[0-9a-f]{6}$" :: [[String]]

checkecl :: Passport -> Bool
checkecl s
  | ecl s == "amb" = True
  | ecl s == "blu" = True
  | ecl s == "brn" = True
  | ecl s == "gry" = True
  | ecl s == "grn" = True
  | ecl s == "hzl" = True
  | ecl s == "oth" = True
  | otherwise = False

checkpid :: Passport -> Bool
checkpid s
  | length res == 0 = False
  | otherwise = True
  where
    res = pid s =~ "^\\d{9}$" :: [[String]]