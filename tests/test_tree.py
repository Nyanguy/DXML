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

print(xml.findall('.//batter'))

defaults = {"id": ["1001","1002"], "sid": ["010", "011", "012"]}
# TreeParser(xml).parse("!batter.ssid", "10#")
TreeParser(xml).parse("item.batters.batter[*].sid", "01#")
TreeParser(xml).parse("item...batter[@id && @sid].xid", "120#", defaults = defaults)
# TreeParser(xml).parse("item...batters.Tag<id:11 && @sid>.xids", "1", defaults = defaults)
ET.dump(xml)