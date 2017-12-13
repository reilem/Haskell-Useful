module Phonebook (
  Name,
  PhoneNumber,
  Entry, mkEntry,name,phone,
  PhoneBook, names, phones, owner,
  Index(findEntry,empty,singleton,(<+>)),
  Assoc,
  byName,byPhone, emptyBook, addToBook, fromEntries,
  number, callerID,
  bill,bob,jeb,val,
  billbook,bobbook,jebbook,valbook,
  Lookup)
where

import Data.List (intercalate, union)

-- - 1. Entry
type Name        = String
type PhoneNumber = [Int]

showPhone :: PhoneNumber -> String
showPhone = intercalate " " . map show

-- -- a. Complete the definitions of Entry, name and phone
data Entry
  = MkEntry Name PhoneNumber
  deriving (Eq,Show)

mkEntry :: Name -> PhoneNumber -> Entry
mkEntry n p = MkEntry n p

name :: Entry -> Name
name (MkEntry n p) = n

phone :: Entry -> PhoneNumber
phone (MkEntry n p) = p


-- 2. Index

class Index i where
  findEntry :: Eq k => k -> i k -> Maybe Entry
  empty     :: Eq k => i k
  singleton :: Eq k => k -> Entry -> i k
  (<+>)     :: Eq k => i k -> i k -> i k

-- a. Complete the definition of Assoc
data Assoc k
  = MkAssoc [(k,Entry)]
  deriving (Eq,Show)

-- b. Complete the instance of Index for Assoc
instance Index Assoc where
  findEntry k (MkAssoc list) =
    do lookup k list
  empty = MkAssoc []
  singleton k e = MkAssoc [(k,e)]
  MkAssoc l1 <+> MkAssoc l2 = MkAssoc $ union l1 l2

-- 3. Complete the definition of PhoneBook, names, phones and owner
data PhoneBook  = MkPhoneBook Entry (Assoc Name) (Assoc PhoneNumber)
  deriving Show


names :: PhoneBook -> Assoc Name
names (MkPhoneBook e n p) = n

phones :: PhoneBook -> Assoc PhoneNumber
phones (MkPhoneBook e n p) = p

owner  :: PhoneBook -> Entry
owner (MkPhoneBook e n p) = e


-- 4. Implement byName and byPhone, emptyBook, addToBook, fromEntries

byName :: Name -> PhoneBook -> Maybe Entry
byName n book = findEntry n (names book)

byPhone :: PhoneNumber -> PhoneBook -> Maybe Entry
byPhone p book = findEntry p (phones book)

emptyBook :: Entry -> PhoneBook
emptyBook e = MkPhoneBook e empty empty

addToBook :: Entry -> PhoneBook -> PhoneBook
addToBook e book =
  MkPhoneBook (owner book)
              (singleton (name e) e <+> (names book))
              (singleton (phone e) e <+> (phones book))

fromEntries :: Entry -> [Entry] -> PhoneBook
fromEntries e lst = MkPhoneBook e ns ps
  where
    ns :: Assoc Name
    ns = foldl (<+>) empty $ map (\ e -> singleton (name e) e) lst
    ps :: Assoc PhoneNumber
    ps = foldl (<+>) empty $ map (\ e -> singleton (phone e) e) lst

-- 5. Implement the callerID function.

data Telephone =
  MkTelephone PhoneNumber (PhoneNumber -> IO ())

number  :: Telephone -> PhoneNumber
number (MkTelephone pn _) = pn

receive :: Telephone -> PhoneNumber -> IO ()
receive (MkTelephone _ r) = r

callerID :: PhoneBook -> Telephone
callerID book =
  MkTelephone (phone $ owner book)
              (\ p ->
                case byPhone p book of
                  Nothing -> do
                    putStrLn ("caller ID: " ++ show p)
                    putStrLn "Ring ring!"
                  Just e  -> do
                    putStrLn ("caller ID: " ++ name e)
                    putStrLn "Ring ring!"
              )

-- 6. Calling someone

call :: PhoneBook -> [Telephone] -> IO ()
call book teles = do
    putStrLn "Who would you like to call?"
    n <- getLine
    case byName n book of
      Nothing -> putStrLn "No such entry!"
      Just e  -> case findTele (phone e) teles of
        Nothing -> putStrLn "The number you dialed does not exist."
        Just t  -> receive t $ phone $ owner book
  where
    findTele :: PhoneNumber -> [Telephone] -> Maybe Telephone
    findTele p []       = Nothing
    findTele p (t:ts)
      | p == (number t) = Just t
      | otherwise       = findTele p ts


-- examples -- do NOT change

bill,bob,jeb,val :: Entry
bill = mkEntry "Bill"      [32,444,123]
bob  = mkEntry "Bob"       [32,444,124]
jeb  = mkEntry "Jebediah"  [32,444,125]
val  = mkEntry "Valentina" [32,444,126]

billbook,bobbook,jebbook,valbook :: PhoneBook
billbook = fromEntries bill [bob,jeb]
bobbook  = fromEntries bob  [bill,jeb]
jebbook  = fromEntries jeb  [bill,bob,val]
valbook  = fromEntries val  [bill,bob,jeb]

telephones :: [Telephone]
telephones = map callerID [billbook,bobbook,jebbook,valbook]

-- 7. Complete the Index instance for Lookup

data Lookup k = MkLookup (k -> Maybe Entry)

instance Index Lookup where
  findEntry k (MkLookup f)       = f k
  empty                         = MkLookup (\ _ -> Nothing)
  singleton k e                 = MkLookup  (\ k' ->
                                    | k == k' = Just e
                                    | otherwise = Nothing
                                  )
  (MkLookup f) <+> (MkLookup g) = MkLookup (f.g)
