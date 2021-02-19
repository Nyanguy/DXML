from typing import List, Dict, Optional, Any, Tuple
from .tokens import Token
from lxml import etree
from dataclasses import dataclass
from enum import Enum


@dataclass
class RunTime:
    root: Optional[str]
    document: Optional[str]
    variables: Dict[str, Any]
    document_tree: etree.ElementTree
    t_stream: List[Tuple[Token, str]]

def __string_clean(l: str) -> str:
    """Cleans the string from whitespaces and comments

    Args:
        l (str): Source string

    Returns:
        str: Cleaned string
    """
    return l.split('//')[0].rstrip() if '//' in l else l.rstrip()


def read_source(config: str, output: Optional[str]) -> Optional[etree.ElementTree]:
    with open(config, "r") as f:
        t_stream = __tokenize(f.read())
    


def execute(config: str, output: Optional[str], defaults: Optional[Dict[str, str]]) -> Optional[etree.ElementTree]:
    """This function can interpret given config file and generate or edit xml tree

    Args:
        config (str): A path to the config file
        output (Optional[str]): A name of the output file. If not specified it would return lxml.ElementTree
        defaults (Optional[Dict[str, str]]): A user dictionary that defines variables that are matched against config

    Returns:
        Optional[etree.ElementTree]: edited XML tree structure
    """
    root   = None
    uvars  = None
    source = None
    current_expr = ""
    with open(config, "r") as f:
        for line in f:
            # skip a comment
            if line.startswith("//"): 
                continue

            if line.capitalize().startswith("DOCUMENT:"):
                source = __string_clean(line.split(":")[1])
            elif line.capitalize().startswith("ROOT:"):
                root  = __string_clean(line.split(":")[1])
            elif line.capitalize().startswith("VARS:"):
                uvars = __string_clean(line.split(":")[1])
            
            current_expr += __string_clean(line)

