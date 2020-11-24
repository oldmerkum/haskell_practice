-- file: ch03/Nullable.hs

data Hm a = Yea a
    | Naw
    deriving (Show)

someBool = Yea True

someString = Yea "something"

wrapped = Yea (Yea "wrapped")
