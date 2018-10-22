#!/usr/bin/python


import urllib3
from flask import Flask
from flask import render_template
from flask import request
from flask import make_response

def getExpansion(pred):
    http = urllib3.PoolManager()
    base_url = "http://localhost:8080/query?"
    return http.request("GET", base_url + pred).data


# Copied from a manual
app = Flask(__name__)

@app.route("/")
def helloDefault():
    value = request.cookies.get('key')
    if value is None:
        value = '0'
    value = str(int(value) + 1)

    resp = make_response(render_template('default.html', value=value))
    resp.set_cookie('key', value)
    return resp
    
app.debug = True
app.run()