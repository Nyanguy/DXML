from typing import Union, List, Tuple
from enum import Enum


class BadExpression(Exception): pass


# You may edit the tokens to customize the language to your likings
class Token(Enum):
    """General token deffinitions"""
    TAG = "TAG"              # This value is unused
    ATTR = "ATTR"            # This value is unused
    PROP = "PROPOSITION"     # This value is unused
    SELECTOR = "."           # Selects everything from a previous statement
    TERM = ";"               # Specifies end of expression
    SKIP = "..."             # Skips certain number of tags
    FINDALL = "!"            # Finds every matching tag from every subtag
    SHIELD = "\\"            # Shields special characters
    OPENBR = "["             # Open predicate brace
    CLOSEBR = "]"            # Close predicate brace
    OPENTAG = "<"            # Open new tag brace
    CLOSETAG = ">"           # Close new tag brace
    SEP = ":"                # Seperate attribute from value
    ALT_SEP = "="            # Alternative option for the above
    OR = "||"                # Disjucntion operator for predicate
    AND = "&&"               # Conjucntion operator for predicate
    REF = "@"                # Reference external variable by key
    WILDCARD = "*"           # RE mask to match tags and attributes by name
    QSIGN = "?"              # Modify only existing entries
    # FILE TOKENS
    VARIABLE = "let"         # Create a new variable
    DOCUMENT = "LOAD"        # Loads new xml document 
    SAVE = "SAVE"            # Saves snapshot of the document
    VARS = "VARS"            # Declares external variables that will be used
    EXPR = "EXPRESSION"      # This value is unused
    FILEPATH = "FILE_PATH"   # This value is unused
    COMMENT = "//"           # A comment statement


def tokenize_file(source: str):
    ptr = 0
    expr_begins = 0
    t_stream = []
    while ptr != len(source):
        if __is_token(source, ptr, Token.SEP.value) or __is_token(source, ptr, Token.ALT_SEP.value):
            pass
        elif __is_token(source, ptr, Token.TERM.value):
            t_stream.append((Token.RAWVALUE, source[expr_begins:ptr]))
            t_stream.append((Token.TERM, Token.TERM.value))
        elif __is_token(source, ptr, Token.VARIABLE.value):
            pass
        elif __is_token(source, ptr, Token.COMMENT.value):
            pass
        elif __is_token(source, ptr, Token.DOCUMENT.value):
            pass
        elif __is_token(source, ptr, Token.ROOT.value):
            pass
        elif __is_token(source, ptr, Token.SAVE.value):
            pass
        elif __is_token(source, ptr, Token.VARS.value):
            pass
        else:
            expr_begins = ptr
            ptr += 1


def tokenize_expression(expr: str) -> List[Union[Tuple[Token, str], Token]]:
    """General tokenizer that will produce resulting token stream

    Args:
        expr (str): A single line of expression

    Raises:
        BadExpression: Given expression contains syntax errors
    """
    tokens = []
    ptr = 0
    in_brace = False
    word_begin = -1
    while ptr != len(expr):
        if not in_brace and __is_token(expr, ptr, Token.SELECTOR.value):
            if word_begin >= 0:
                tokens.append((Token.TAG, expr[word_begin:ptr].strip()))
                word_begin = -1
            if len(expr) > ptr + len(Token.SKIP.value) and __is_token(expr, ptr, Token.SKIP.value):
                tokens.append(Token.SKIP)
                ptr += 2
        elif not in_brace and __is_token(expr, ptr, Token.FINDALL.value):
            tokens.append(Token.FINDALL)
            word_begin = -1
        elif __is_token(expr, ptr, Token.SHIELD.value):
            word_begin += expr[ptr]
            ptr += 1
        elif __is_token(expr, ptr, Token.OPENBR.value):
            in_brace = True
            tokens.append((Token.TAG, expr[word_begin:ptr].strip()))
            tokens.append(Token.OPENBR)
            word_begin = -1
        elif __is_token(expr, ptr, Token.CLOSEBR.value):
            in_brace = False
            tokens.append((Token.PROP, expr[word_begin:ptr].strip()))
            tokens.append(Token.CLOSEBR)
            word_begin = -1
        elif __is_token(expr, ptr, Token.OR.value):
            tokens.append((Token.PROP, expr[word_begin:ptr].strip()))
            tokens.append(Token.OR)
            word_begin = -1
            ptr += 1
        elif __is_token(expr, ptr, Token.AND.value):
            tokens.append((Token.PROP, expr[word_begin:ptr].strip()))
            tokens.append(Token.AND)
            word_begin = -1
            ptr += 1
        elif __is_token(expr, ptr, Token.REF.value):
            if not in_brace:
                raise BadExpression(
                    "'@' lookup operator can be only inside the braces (<>,[])")
            tokens.append(Token.REF)
            word_begin = -1
        elif __is_token(expr, ptr, Token.SEP.value) or __is_token(expr, ptr, Token.ALT_SEP.value):
            tokens.append((Token.ATTR, expr[word_begin:ptr].strip()))
            tokens.append(Token.SEP)
            word_begin = -1
        elif __is_token(expr, ptr, Token.WILDCARD.value):
            tokens.append((Token.WILDCARD, len(expr[word_begin:ptr].strip())))
            if ptr == len(expr) - 1:
                tokens.append((Token.ATTR, expr[word_begin:ptr].strip()))
        elif __is_token(expr, ptr, Token.OPENTAG.value):
            in_brace = True
            tokens.append((Token.TAG, expr[word_begin:ptr].strip()))
            tokens.append(Token.OPENTAG)
            word_begin = -1
        elif __is_token(expr, ptr, Token.CLOSETAG.value):
            in_brace = False
            tokens.append((Token.PROP, expr[word_begin:ptr].strip()))
            tokens.append(Token.CLOSETAG)
            word_begin = -1
        elif __is_token(expr, ptr, Token.QSIGN.value):
            tokens.append(Token.QSIGN)
            tokens.append((Token.ATTR, expr[word_begin:ptr].strip()))
            word_begin = -1
        elif __is_token(expr, ptr, Token.TERM.value):
            if tokens[-1] != Token.SEP:
                # If attribute is specified at the end
                tokens.append((Token.ATTR, expr[word_begin:ptr].strip()))
            else:
                # If assignment is at the end of the statement
                tokens.append((Token.PROP, expr[word_begin:ptr].strip()))
            tokens.append(Token.TERM)
            word_begin = -1
        else:
            if expr[ptr] != " " and word_begin == -1:
                word_begin = ptr
        ptr += 1
    __tokens_check_opcl(tokens)
    return tokens


def __clean_expression(e: str) -> str:
    pass


def __is_token(src: str, ptr: int, token: str) -> bool:
    """Checks if the token is present current position

    Args:
        src (str): An input file string 
        ptr (int): Pointer to the current location in the file
        token (str): Token that will be checked

    Returns:
        bool: Whether token is present
    """
    return src[ptr] == token[0] and src[ptr:ptr+len(token)] == token


def __tokens_check_opcl(tokens: List[Union[Tuple[Token, str], Token]]):
    i = 0
    for t in tokens:
        if t == Token.OPENBR or t == Token.OPENTAG:
            i += 1
        elif t == Token.CLOSEBR or t == Token.CLOSETAG:
            i -= 1
    if i != 0:
        raise BadExpression(
            "Missmatched number of opening and closing braces. Please check your expression.")
