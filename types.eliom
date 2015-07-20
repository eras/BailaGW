{shared{
type timestamp = string deriving (Json)

type contents =
  | Text of string
  | Image of string
deriving (Json)

type message = {
  timestamp : timestamp;
  src       : string;
  dst       : string;
  contents  : contents;
} deriving (Json)

type messages = message list deriving (Json)

type range = (int * int) deriving (Json)

type fragment =
  | Url of string
deriving (Json)

type processed_message = {
  pm_meta    : (range * fragment) list;
  pm_message : message;
} deriving (Json)
}}
