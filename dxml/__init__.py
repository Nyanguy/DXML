from typing import Callable, Dict, Optional
from .TreeParser import TreeParser
from lxml import etree

def __string_clean(l: str) -> str:
    """Cleans the string from whitespaces and comments

    Args:
        l (str): Source string

    Returns:
        str: Cleaned string
    """
    return l.split('//')[0].rstrip() if '//' in l else l.rstrip()

def execute(config: str, output: Optional[str], defaults: Optional[Dict[str, str]]) -> Optional[etree.ElementTree]:
    """This function can interpret given config file and generate or edit xml tree

    Args:
        config (str): A path to the config file
        output (Optional[str]): A name of the output file. If not specified it would return lxml.ElementTree
        defaults (Optional[Dict[str, str]]): A user dictionary that defines variables that are matched against config

    Returns:
        Optional[etree.ElementTree]: edited XML tree structure
    """
    rootless = False
    source   = None
    current_expr = ""
    with open(config, "r") as f:
        for line in f:
            # skip a comment
            if line.startswith("//"): 
                continue
            if line.capitalize().startswith("DOCUMENT:") or line.capitalize().startswith("ROOT:"):
                source = __string_clean(line.split(":")[1])
                rootless = line.capitalize().startswith("DOCUMENT:")
            
