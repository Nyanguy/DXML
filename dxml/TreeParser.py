from typing import Union, List, Optional, Tuple, Dict
from .tokens import Token, tokenize_expression, BadExpression
import lxml.etree as ET
import re


class BadValue(Exception): pass
class Missmatched(Exception): pass


def check_order(token, rules: Dict, stack: List):
    if not stack:
        return False
    try:
        return rules[token] <= rules[stack[-1]]
    except KeyError:
        return False
    except TypeError:
        return False


def infix_to_postfix(token_stream: List, rules: Dict) -> List:
    """Changes infix notation to postfix.
    token_stream => A list of tokens.
    rules => is a dict with operations as keys and order as values.
             Higher order operations evaluates earlier.
    """
    stack = []
    result = []
    derived_type = [type(k) for k in rules][0]
    for token in token_stream:
        if token == '(':
            stack.append(token)
        elif token == ')':
            while stack[-1] != '(':
                result.append(stack.pop())
            stack.pop()
        elif isinstance(token, derived_type) and token in rules:
            while stack and check_order(token, rules, stack):
                result.append(stack.pop())
            stack.append(token)
        else:
            result.append(token)

    while stack:
        result.append(stack.pop())
    return result

class TreeParser:
    """A useful tool to quickly modify a known XML Tree structure"""
    def __init__(self, tree: ET.Element, stage_name: Optional[str] = ""):
        self.tokens = []
        self.name   = stage_name
        self.tree   = tree

    
    def __tokens_set_ref(self, defaults: Dict[str, Union[List[str], str]]):
        """[..., @REF, T::PROP, ...] -> [..., T::ATTR, T::PROP, ...]"""
        i = 0
        while i < len(self.tokens):
            if self.tokens[i] == Token.REF:
                next_token = self.tokens[i+1]
                if isinstance(next_token, tuple) and next_token[0] == Token.PROP:
                    if next_token[1] not in defaults:
                        raise BadExpression(f"Trying to access @{next_token[1]}, but it is not present in the dictionary")
                    self.tokens[i]   = (Token.ATTR, next_token[1])
                    self.tokens[i+1] = (Token.PROP, defaults[next_token[1]])
            i+=1


    def parse(self, expr: str, value: Optional[Union[str, List[str]]] = None, const: Optional[bool] = False, defaults: Optional[Dict[str, Union[str, List[str]]]] = None) -> Union[ET.Element, List[ET.Element]]:
        f'''
        Parses XML tree according to the `expr` and puts the `value` at the end.
        Operators:
            {Token.SELECTOR}   -- A common seperator that takes the first found tag
            {Token.SKIP} -- A seperator that skips tags until the first match was found
            {Token.FINDALL}   -- Specifies to find matches across the whole document
            {Token.QSIGN}   -- Change only existing occurances of the attribute
            {Token.WILDCARD}   -- Wildcards/masks that can be applied anywhere in the expression except tag creation
            {Token.REF}   -- Lookup inside the defaults dictionary
            {Token.SEP}|{Token.ALT_SEP} -- A seperator that specifies Attribute and Value
            {Token.SHIELD}   -- Shielding of special characters
            {Token.TERM}   -- End of the expression
            N{Token.OPENTAG}A{Token.SEP} V {Token.AND} {Token.REF}A{Token.CLOSETAG}T  -- Create a tag and select it where 
                N  -- tag name 
                A  -- attribute name
                V  -- attribute value
                T  -- tag text value
                {Token.AND} -- Any devider would do
                {Token.REF}  -- Inserts dictionary value by a given key
            {Token.OPENBR}A{Token.SEP} V {Token.OR} A{Token.SEP} V {Token.AND} V {Token.AND} {Token.REF}A{Token.CLOSEBR}  -- A conditional predicate that filters tags where
                A  -- attribute name
                V  -- attribute value; can be matched in conjunction with A, or on its own
                {Token.OR} -- disjunction operator
                {Token.AND} -- conjunction operator
                {Token.REF}  -- Checks dictionary by a given key
        Example with the default syntax:
            xml = etree.fromstring(r"/home/user/xmltest.xml")
            tp = TreeParser(tree = xml)

            """
            Creates attribute inside the given tag.
            <Axes AxisAttr = "HVol1">
            ...
            </Axes>
            """
            tp.parse(value = "HVol1", expr = "Axes.AxisAttr") 


            """ 
            Creates "BVol#" attribute where # is an enumeration of encountered tags.
            <Axes>
                ...
                    <SomeNestedTag Attr="BVol0"> 
                    <SomeNestedTag Attr="BVol1"> 
                    <SomeNestedTag Attr="BVol2"> 
                ...
            </Axes>
            """
            tp.parse(value = "BVol#", expr = "Axes...Attr") 

            """ 
            Creates "Lambda" attribute for every given tag Axis.
            <Axes>
                <Axis Lambda="12"> 
                <Axis Lambda="12"> 
                <Axis Lambda="12"> 
            </Axes>
            """
            tp.parse(value = "12", expr = "Axes.Axis[*].Lambda", const = True)
        '''
        # cleans the expression
        print(expr)
        expr = ''.join(map(lambda s: s.strip(), expr.splitlines()))
        print(expr)
        if ';' not in expr:
            expr += ';'
        self.tokens = tokenize_expression(expr)
        tb = f"{self.name}::Tag.item"
        current_tag = [self.tree]
        print(self.tokens)

        if self.tokens[-1] != Token.TERM:
            raise BadExpression(f"Expression must end with ';'")

        # if self.tokens[-1][0] not in [Token.ATTR, Token.TAG]:
        #     raise BadExpression(f"Expression should end with an attribute or a tag, not {self.tokens[-1]}")
        
        if Token.REF in self.tokens: 
            if defaults is None:
                raise BadValue(f"Expression contains external references '@', but no 'defaults' dictionary was provided.")
        
            self.__tokens_set_ref(defaults)

        if isinstance(value, str) and "#" not in value and const == False:
            const = True

        wildcards = []          # State variable that contains wildcard tokens that will be applied to the text
        mask = ""               # State variable that is applied when performing searches
        entity_required = False # State variable that dictates whether to check an existence of attribute or not
        tag_find_many   = False # State variable that dictates whether find many of 1 tag when Token::TAG is encountered
        i = 0                   # Current position in the token stream
        while i != len(self.tokens):
            token = self.tokens[i]

            if isinstance(token, tuple):
                token, t_value = token 
                
                if token == Token.TERM:
                    current_tag = [self.tree]

                if token == Token.WILDCARD:
                    wildcards.append(t_value)

                elif token == Token.TAG:
                    text = self.__insert_wildcard(wildcards, t_value)
                    if self.tokens[i+1] == Token.OPENTAG:
                        for tag in current_tag:
                            for _ in range(self.__number_of_ref_elements(i+1, defaults)):
                                tag.append(ET.Element(t_value))
                        tag_find_many = True
                    if tag_find_many:
                        current_tag = sum([f for f in map(lambda x: x.findall(f"{mask}{t_value}"), current_tag) if f is not None], [])
                        tag_find_many = False
                    else:
                        current_tag = [f for f in map(lambda x: x.find(f"{mask}{t_value}"), current_tag) if f is not None]
                    if len(current_tag) == 0:  
                        raise BadValue(f"{tb} => {t_value}\n\t No such tag in the config")
                    tb += f"::{current_tag[0].tag}"
                    mask = ""

                elif token == Token.PROP:
                    # Should obey the structure [..., ATTR, SEP, PROP]
                    if self.tokens[i-1] == Token.SEP and self.tokens[i-2][0] == Token.ATTR:
                        self.assign_value(
                            current_tag, [t_value], wildcards, self.tokens[i-2][1], const, tb, entity_required)
                    else:
                        raise BadExpression(f"Failed to create value {t_value}. Token::ATTR is missing.")
                    wildcards = []

                elif token == Token.ATTR:
                    print(f"IN ATTR: {self.tokens[i:]}")
                    if value is not None: 
                        self.assign_value(current_tag, value, wildcards, t_value, const, tb, entity_required)
                        wildcards = []
                
            elif token == Token.SKIP: 
                tb += f"::{Token.SKIP.value}"
                mask = './/'
            
            elif token == Token.FINDALL:
                tb += f"::{Token.FINDALL.value}"
                mask = './/'
                tag_find_many = True

            elif token == Token.OPENTAG: 
                i += self.__default_tag_attributes(current_tag, self.tokens[i+1:])
                wildcards = []

            elif token == Token.QSIGN: 
                entity_required = True

            elif token == Token.OPENBR:
                current_tag = sum([f for f in map(lambda x: x.getparent().findall(x.tag), current_tag)], []) # Find all instances (MANOIDS SON!)
                current_tag, j = self.__match_predicate(current_tag, self.tokens[i+1:])
                i += j+1 # +1 to account for closing brace
                wildcards = []
            i+=1
        
        return current_tag


    def __default_tag_attributes(self, current_tag: Union[List[ET.Element], ET.Element], tokens: List[Token]):
        tb = f"{self.name}::Tag.attr"
        stack = []
        i = 0
        for i, token in enumerate(tokens):
            if isinstance(token, tuple):
                token, t_value = token
                
                if token == Token.ATTR:
                    stack.append(t_value)

                elif token == Token.PROP:
                    if not stack:
                        continue
                        # raise BadValue(f"{tb} = {t_value}\n\t Stack is empty. Please specify the attribute name")
                    attr = stack.pop()
                    if not isinstance(t_value, list):
                        for tag in current_tag:
                            tag.attrib[attr] = t_value
                    else:
                        for i,tag in enumerate(current_tag):
                            tag.attrib[attr] = t_value[i]

            elif token == Token.AND: pass
            elif token == Token.OR: pass
            elif token == Token.CLOSEBR: break
        return i


    def __match_predicate(self, current_tag: Union[List[ET.Element], ET.Element], tokens: List[Token]) -> Tuple[ET.Element, int]: 
        tb = f"{self.name}::Tag.attr"
        stack = []
        wildcards = []
        matched_attr = []
        opened_brace = False
        i = 0
        for i, token in enumerate(tokens):
            if isinstance(token,tuple):
                
                token, t_value = token
                
                if token == Token.WILDCARD:
                    wildcards.append(t_value)
                
                elif token == Token.ATTR:
                    if not opened_brace:
                        stack.append('(')
                        opened_brace = True
                    else:
                        if stack[-1] in [Token.OR, Token.AND]:
                            op = stack.pop()
                            stack.append(')')
                            stack.append(op)
                        else:
                            stack.append(')')
                        stack.append('(')

                    text = self.__insert_wildcard(wildcards, t_value)
                    wildcards = []
                    matched = [(t, x) for t in current_tag for x in t.attrib if re.match(text, x)]
                    if matched: 
                        matched_attr = matched
                    else: 
                        raise BadValue(f"{tb} = {t_value}\n\t No such attribute in the tag: {current_tag[0]}")
                
                elif token == Token.PROP:
                    text = self.__insert_wildcard(wildcards, t_value)
                    wildcards = []
                    tmp = set()
                    if matched_attr:
                        for tag, attr in matched_attr:
                            if isinstance(text, list):
                                for v in text:
                                    if re.match(v, tag.attrib[attr]):
                                        tmp.add(tag)
                            else:
                                if re.match(text, tag.attrib[attr]):
                                        tmp.add(tag)
                    else:
                        stack.append('(')
                        opened_brace = True
                        for tag in current_tag:
                            if tag.attrib == {} and text == ".*":
                                tmp.add(tag)
                            else:
                                for _,v in tag.attrib.items():
                                    if re.match(text, v):
                                        tmp.add(tag)
                    stack.append(tmp)

            elif token == Token.AND:
                stack.append(Token.AND)
            
            elif token == Token.OR: 
                stack.append(Token.OR)
            
            elif token == Token.CLOSEBR:
                stack.append(')')
                opened_brace = False
                stack = self.__evaluate_stack(infix_to_postfix(stack, {Token.AND:1, Token.OR:0}))
                break
        return stack, i
    

    def __evaluate_stack(self, stack: List) -> List[ET.Element]:
        estack = []
        for token in stack:
            if token == Token.AND:
                r = estack.pop()
                estack[-1] = estack[-1].intersection(r)
            elif token == Token.OR: 
                r = estack.pop()
                estack[-1] = estack[-1].union(r)
            else: 
                estack.append(token)
        return list(estack[0])


    def __number_of_ref_elements(self, offset: int, default: Dict[str, Union[str, List[str]]]) -> int:
        num_of_tags = 1
        for i in range(offset, len(self.tokens)):
            if isinstance(self.tokens[i], tuple) and self.tokens[i][0] == Token.PROP:
                value = self.tokens[i][1]
                if isinstance(value, list):
                    if num_of_tags == 1:
                        num_of_tags = len(value)
                    if num_of_tags != len(value):
                        raise Missmatched("Wrong number of elements inside the defaults. They should be the same size.")
                    
            elif self.tokens[i] == Token.CLOSETAG:
                break
        return num_of_tags


    def assign_value(self, current_tag: List[ET.Element], value: str, wildcards: List, t_value: str, const: bool, tb: str, entity_required: bool):
        text = self.__insert_wildcard(wildcards, t_value)
        if const:
            value = [value for _ in range(len(current_tag))]
        else:
            if "#" in value:
                value = [value.replace("#", str(j))
                                        for j in range(len(current_tag))]
            else:
                if len(value) != len(current_tag):
                    raise Missmatched(
                        f"{tb} >>\n\tLength of values does not match length of tags: {len(value)}(values) != {len(current_tag)}(tags)\n ")
                value = [value[i] for i in range(len(current_tag))]

        if wildcards:       # FOR *  and  ?
            empty_match = True
            tag = None
            for j, tag in enumerate(current_tag):
                for k in tag.attrib:
                    if re.match(text, k):
                        empty_match = False
                        tag.attrib[k] = value[j]

            if tag is None:
                raise BadValue(
                    f"{tb}::{current_tag} >> EMPTY\n\t Failed to retrieve a element child.")

            elif empty_match:
                raise BadValue(
                    f"{tb}::{tag.tag} = {text}\n\t There is no match for the attribute")

        elif entity_required:         # FOR ?
            for j, tag in enumerate(current_tag):
                if text in tag.attrib:
                    tag.attrib[text] = value[j]
                else:
                    raise BadValue(
                        f"{tb}::{tag.tag} = {text}\n\t There is no match for the attribute")
        else:               # FOR PLANE ATTR
            for j, tag in enumerate(current_tag):
                tag.attrib[text] = value[j]


    def __insert_wildcard(self, wildcards: List, text: str) -> str:
        if wildcards:
            for shift,star in enumerate(wildcards):
                text = text[:star+shift] + '.*' + text[star+shift:]
        return text
