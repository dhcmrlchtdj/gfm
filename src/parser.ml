type block =
    | Open
    | Document of block list
    | Paragraph of block
    | BlockQuote of block
    | List of block list
    | ListItem of block list

