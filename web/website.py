#!/usr/bin/python


import urllib3
from flask import Flask
from flask import render_template
from flask import request
from flask import make_response


# Copied from a Stack Exchange answer to get unicode working

import sys
if sys.version_info.major < 3:
    reload(sys)
sys.setdefaultencoding('utf8')


# Function to retrive the expansion of a predicate from the
# Scheme webservice

def getExpansion(pred):
    http = urllib3.PoolManager()
    base_url = "http://localhost:8080/query?"
    return http.request("GET", base_url + pred).data


# Copied from a manual
app = Flask(__name__)


# "Hello world" test page

@app.route("/hello")
def hello():
    return "Hello world!"


# Page to test cookies (should show an increasing number on each
# reload)

@app.route("/cookiesTest")
def cookiesTest():
    value = request.cookies.get('key')
    if value is None:
        value = '0'
    value = str(int(value) + 1)

    resp = make_response(render_template('default.html',
                                         value=value))
    resp.set_cookie('key', value)
    return resp


## PARSE PAGE AND HELPERS

# A pair of functions for turning the (string) data stored in
# the recent_parses cookies into a list and back

def getQueries(recent):
    if recent is None or recent == "":
        return []
    return recent.split(";")

def getRecent(queries):
    length = len(queries)

    if length > 10:
        queries = queries[length-10:]

    ret = ""
    for query in queries:
        ret += ";"
        ret += query

    return ret[1:]

# Parse page

@app.route("/parse/")
def parse():
    query = request.args.get('input')

    recent = request.cookies.get('recent_parses')
    queries = getQueries(recent)
    if not (query is None or query == ""):
        queries.append(query)
    recent = getRecent(queries)

    queries.reverse()
    queries = map(lambda q : q + ": " + getExpansion(q), queries)
    resp = make_response(render_template('recent.html',
                                         queries=queries))

    resp.set_cookie('recent_parses', recent)
    return resp
    

app.debug = True
app.run()