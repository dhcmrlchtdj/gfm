type span =
    | Opening of string
    | Closing of string
    | SelfContained of string
    | Text of string
