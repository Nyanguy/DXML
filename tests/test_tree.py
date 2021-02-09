import lxml.etree as ET
from dxml.TreeParser import TreeParser

xml = ET.fromstring("""
<items>
	<item id="0001" type="donut">
		<name>Cake</name>
		<ppu>0.55</ppu>
		<batters>
			<batter id="1001">Regular</batter>
			<batter id="1002">Chocolate</batter>
			<batter id="1003">Blueberry</batter>
		</batters>
		<topping id="5001">
			<batter id="1003">Blueberry</batter>
		</topping>
		<topping id="5002">Glazed</topping>
		<topping id="5005">Sugar</topping>
		<topping id="5006">Sprinkles</topping>
		<topping id="5003">Chocolate</topping>
		<topping id="5004">Maple</topping>
		<batter id="1003">Blueberry</batter>
	</item>
	<batter id="1003">Blueberry</batter>
</items>
""")

defaults = {"id": ["1001","1002"], "sid": ["010", "011", "012"]}
tp = TreeParser(xml)
tp.parse("!batter.ssid", "10#")
tp.parse("item.batters.batter[*].sid", "01#")
tp.parse("item.topping[id=5002].sid:11")
tp.parse("item...batter[@id && @sid].xid", "120#", defaults = defaults)
tp.parse("item...batters.Tag<id:11 && @sid>.xids", "1", defaults = defaults)
tp.parse(
    "item...batter[*].newTag<>.AnoterTag<>.xids", "1", defaults=defaults)
ET.dump(xml)
