#!/usr/bin/env python

# http://www.wikidot.com/doc:api-methods

import ConfigParser
import os
import sys
import xmlrpclib

APP = 'page_content.py'
SITE = 'hyperpolyglot'

URI_FMT = 'https://{app}:{access_key}@www.wikidot.com/xml-rpc-api.php'

CONFIG_FILE = os.path.join(os.getenv('HOME'), '.wikidot')
SECTION_API = 'API'
KEY_READONLY_ACCESS_KEY = 'readonly_access_key'


def load_config():

    if os.path.exists(CONFIG_FILE):
        config = ConfigParser.SafeConfigParser()
        config.read(CONFIG_FILE)

    else:
        raise Exception('Config file not found: {}'.format(CONFIG_FILE))

    return config


def page_content(page, output_stream):

    config = load_config()
    access_key = config.get(SECTION_API, KEY_READONLY_ACCESS_KEY)
    uri = URI_FMT.format(app=APP, access_key=access_key)

    sp = xmlrpclib.ServerProxy(uri)
    p = sp.pages.get_one({'site': SITE,
                          'page': page})
    output_stream.write(p['content'])


if __name__ == '__main__':

    if len(sys.argv) == 2:
        page = sys.argv[1]
        page_content(page, sys.stdout)

    else:
        raise Exception('USAGE: page_content.py PAGE')
