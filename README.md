![img](img/dxml.jpg)


A declarative xml tree DSL capable of edition and generation written in Python

## Why?

Lxml is great and fast library to manipulate xml tree structure! However, I find its API outdated and in need of a refresh. Furthermore, xml is very widespread and its users might be clueless about programming, and they don't want to learn a general purpose programming language to build or edit xml documents. I believe that the **data should be** configured by the input values **not the source code**. 


## Installation

### Pip
```sh
pip install dxml
```

### Build
```sh
git clone https://gitlab/omen/.git
cd DXML && pip install .
```

## Usage 

From shell
```sh
dxml my_config.dxml -o generated.xml
```

From python
```python 
from dxml import TreeParser, execute
````

### dxml internal working

|entity|description|
|:----:|:---------:|
|TAG |Specifies selected tag name in the xml tree|
|ATTR|Specifies attribute name. Usually stated at the end of the expression or inside the predicate before `:`|
|PROP|Specifies value of the attribute. Can be attached to the specific attribute or to any one if left without specification|


|operator|spec|description|
|:------:|:--:|:---------:|
|`.`  |only pre TAG   |A common seperator that takes the first found TAG|
|`...`|only pre TAG   |A seperator that skips TAGs until the first match was found|
|`!`  |only pre TAG   |Specifies to find matches across all children|
|`?`  |only after ATTR|Change only existing occurances of the ATTR|
|`<>` |only after TAG |Brace. Create a TAG(s) and select it/them|
|`[]` |only after TAG |Brace. A conditional predicate that filters TAGs|
|`@`  |only inside braces|Lookup inside the defaults dictionary|
|`:`\|`=`|only inside braces & after ATTR|A seperator that specifies Attribute and Value|
|`\|\|` |only inside braces & after PROP|disjunction operator|
|`&&` |only inside braces & after PROP|conjunction operator|
|`*`  |applyed anywhere except <ATTR:> and TAG|Wildcards/masks that can be applied anywhere in the expression except TAG creation|
|`\`  |applyed anywhere|Shiedling of special characters|


You may redefine the syntax as you please! Just look into the `TreeParser::Token`.

**TreeParser** is a class that parses XML tree structure by using a simple DSL (_Domain Specific Language_) and changes it's values according to the supplied command. Document is changed by reference that is supplied to the constructor.

**CONFIG USAGE EXAMPLE**
```py
import dxml

my_variables = {
    ...
}

# Running the following command will produce the xml file
dxml.execute("/path/to/my_config", output = "/path/to/final.xml", defaults = my_variables)

# alternatively, this will return the config as lxml.etree.ElementTree
my_xml = dxml.execute("/path/to/my_config", defaults = my_variables)
...
```

**SINGLE-LINE USAGE EXAMPLE**
```python
from dxml import TreeParser
import lxml.etree as ET
xml = etree.parse(r"/home/user/xmltest.xml")
tp = TreeParser(tree = xml)

"""
Creates attribute inside the given tag.
<Axes AxisAttr = "Value">
...
</Axes>
"""
tp.parse(expr = "Axes.AxisAttr", value = "Value") 


"""
Multple conditions for attribute. Find tags where `Name` startswith `ETH` or `BTC` and any attribute in the tag contains `Huobi.`. 
Also, `#` in value stands for enumeration. If you want to pass it literally, then use argument `const = True`.
<Axes>
    <Axis Players="John/Frank"             Match="1v1" ... Attr="Val0"/>
    <Axis Players="John/Mary,Bobby"        Match="1v2" ... Attr="Val1"/>
    <Axis Players="Cary/Frank,Rose"        Match="1v2" ... Attr="Val2"/>
    <Axis Players="John,Carry/Sally,Bobby" Match="2v2" ... />
    ...
    <Axis Players="Cary/Rose"              Match="1v1" ... Attr="Val4"/>
</Axes>
"""
tp.parse(expr = "Axes.Axis[Players: John*||Cary* && 1v*].Attr", value = "Val#") 


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
    <Axis Lambda="42"> 
    <Axis Lambda="42"> 
    <Axis Lambda="42"> 
</Axes>
"""
tp.parse(value = "42", expr = "Axes.Axis[*].Lambda", const = True)

# Perform a global search which results with tag mattching on every level. 
# All <Exchange ...> inside Axis will be changed. If you remove Axis, it will look across the whole document
tp.parse(expr = "Axis!Exchange[xS: ETH/USD].Name", value = "ETH/USDT")

# Supply default arguments as a dictionary
defs = {'Couple': 'John/Marry', 'otherAttr': '184'}
tp.parse(expr = "Vacations.Vacation[@Couple && @otherAttr].inLove", value = "True", defaults = defs)
tp.parse(expr = "Vacations.Vacation<@Couple && attr: 42>")

# Also supports multiple values match!
defs = {'Couple': ['John/Marry', 'Chris/Jully'], 'otherAttr': '184'}
tp.parse(expr = "Vacations.Vacation[@Couple && @otherAttr].inLove", value = "True", defaults = defs)

```

Now let's talk about `expr` argument.
Path describes a dynamic XML tree treversal. It describes an end point of where the `value` should be written.
```c
EXAMPLE 1: Axes.Axis.VolumeA  //A standart treversal
```
The last argument always points to the **TAG ATTRIBUTE**.
What if XML structure is very nested and, say, you don't want to write down it all the way.                          

### Enter the **`...`** operator.

```c
EXAMPLE 2: Axes...VolumeA  //A skip treversal
```

In this example the reference to a variable will be placed in the **FIRST ENCOUNTERED TAG** with this attribute.
Meaning if you have **Axis.Level.VolumeA** and **Axis.VolumeA** the ***latter will be chosen***.


### Conditional tag multiplication.

So far we have covered only the instances of a single tag look-up. For selecting a specific *set* of tags by an argument value use **`[P]`** where `P` is a predicate.
```c
EXAMPLE 3: Exchanges.Exchange[NASDAQ]...Pair[Name:USD||RUB].APP*
//                           └──┬───┘       └─────┬───────┘    │
//                              1                 2            3
```                                  
1. Here we specify a value of some attribute. By default it will try to match the value with ***every tag*** argument.
2. Let's break down what is happening in there. If you want to specify the name of the attribute you can use a syntax like `A: P` where *A* is an attribute name and *P* is a predicate (Selects Currency tag with an attribute Name equal to "BTC").
3. Note the `*`. It is a wildcard that selects every attribute that starts with **BlockPercent**

But if you want to bring multiple values you can use _"||"_ or _"&&"_ for disjunction and conjunction respectively.    In the example above we specify that Currency attribute "Name" can be equal to "BTC" or "USDT".                    

Additionally, if your document contains several tags with the same name and you want to iterate through all of them, you might use a general wildcard:

```c 
EXAMPLE 4: Tag.Levels[*].Level.SomeAttr
//                    │
//  "Always True" condition that will include every tag
```
This form of expression means that tag might contain _Any attribute_ with _Any value_. If a given tag contains no attributes, it would also comply to that rule.


### Create a tag `<>`.

It is also possible to add custom tags to the XML tree structure with `<>`. Below is an example of tag creation and inserting there an attribute.

```c
EXAMPLE 5: Tag.Levels[*].CustomTag<>.MyAttribute
//                                │           │
//  1.Creates a tag with a name "CustomTag"   │
//  2.Inserts an attribute to the newly created tag
```

Note, that as an exception, it is also possible to finish the expression with tag creation

```c
EXAMPLE 6: Tag.Levels[*].CustomTag<>
```

You might also supply default tag attributes:
```c
EXAMPLE 7: Tag.Levels[*].CustomTag<attr: value && otherAttr: 10>
```

### Binary operations.

```c
EXAMPLE 8: Camp.Child[Marry* && myW*dNam*: John||Ale* && my*Age*:1*].Fed?
```
Here we specify multiple selectors. To combine them in one statement you can use _"&&"_ and _"||"_. Note that wildcards `*` are also applicable here (for both attribute and value).

Just like in the example above we start to match every attribute in *Axis* with the *ETH** 

Note the `?`. It explicitly tells the compiler to change only known occurrences of the attribute. If the attribute was not found, it will be skipped. But if **no tag contained the attribute** it will raise an error.        

`&&` stands for **conjunction** and `||` for **disjunction**. Obviously **conjunction has a higher priority** and the evaluation happens from left to right.
A user can also drop a name of an attribute so it will behave like the "*Exchange*" query from the "Example 1".


### `@` external default references

You can supply a dictionary of default attributes to the `TreeParser::parse(...defaults: Optional[Dict[str, str]])`. After that you may reference the values in your expression by using `@` operator in front of your key name. Note that referencing is only allowed in predicates (`<>`, `[]`). In tag creation, parser would generate a new attribute matching your key and value. Likewise when matching a predicate, it would try to match the value by keys. 

```c
EXAMPLE 9: Levels.Level[@myAttr].Lambda
//                      │
// External reference to the {...,'myAttr': 'value',...}
```

Value associated with key can be a `str` or a `List[str]`. Depending on the context of a reference, whether it's a tag creation or matching, values of list will behave slightly differently:
* `<@listVals && @otherList>` -- `listVals` can be of any length as long as it has the same length as `otherList`
* `[@listVals && @otherList]` -- both references' values can be of any length. It would try to match them in compliance with a predicate. 

Search for multiple conditions with a list. Same as explicitly using `||`:
```c
EXAMPLE 9: Levels.Level[@Name].Value
//                      │
// External reference to the {...,'Name': ['Marry', 'John'],...}
```


Create multiple tags with lists of values: 
```c
EXAMPLE 10: Levels.Level<@xS && @Value>
// where xS: [Marry, John]
//    Value: [12, 10]
```

It is ok to combine lists with constants (they will duplicate):
```c
EXAMPLE 10: Levels.Level<@xS && @Value>
// where xS: [Marry, John]
//   Value: 12
```

Following input will produce an error:
```c
EXAMPLE 11: Levels.Level<@xS && @Value>
// where xS: [Marry, John]
//   Value: [12, 10, 13]
```