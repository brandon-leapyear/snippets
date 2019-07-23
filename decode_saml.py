#!/usr/bin/env python3

import base64
import sys
import xml.dom.minidom

response = sys.argv[1]
decoded = base64.b64decode(response)
saml_xml = xml.dom.minidom.parseString(decoded)
print(saml_xml.toprettyxml())
