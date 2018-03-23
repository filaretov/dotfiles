#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This script is a simple wrapper which prefixes each i3status line with custom
# information. It is a python reimplementation of:
# http://code.stapelberg.de/git/i3status/tree/contrib/wrapper.pl
#
# To use it, ensure your ~/.i3status.conf contains this line:
#     output_format = "i3bar"
# in the 'general' section.
# Then, in your ~/.i3/config, use:
#     status_command i3status | ~/i3status/contrib/wrapper.py
# In the 'bar' section.
#
# In its current version it will display the cpu frequency governor, but you
# are free to change it to display whatever you like, see the comment in the
# source code below.
#
# © 2012 Valentin Haenel <valentin.haenel@gmx.de>
#
# This program is free software. It comes without any warranty, to the extent
# permitted by applicable law. You can redistribute it and/or modify it under
# the terms of the Do What The Fuck You Want To Public License (WTFPL), Version
# 2, as published by Sam Hocevar. See http://sam.zoy.org/wtfpl/COPYING for more
# details.

import sys
import json
import time
from subprocess import check_output
from time import localtime, strftime

nord_colors = {
          "nord0"  : "#2E3440"
        , "nord1"  : "#3B4252"
        , "nord2"  : "#434C5E"
        , "nord3"  : "#4C566A"
        , "nord4"  : "#D8DEE9"
        , "nord5"  : "#E5E9F0"
        , "nord6"  : "#ECEFF4"
        , "nord7"  : "#8FBCBB"
        , "nord8"  : "#88C0D0"
        , "nord9"  : "#81A1C1"
        , "nord10" : "#5E81AC"
        , "nord11" : "#BF616A"
        , "nord12" : "#D08770"
        , "nord13" : "#EBCB8B"
        , "nord14" : "#A3BE8C"
        , "nord15" : "#B48EAD"
        }
colors = {
        "foreground" : "#ffffff"
        ,
        }

def i3_json(name, text, color=None, bg=None, border=None, min_width=None,
        align=None, urgent=None, instance=None,
        separator=None, separator_block_width=None):
    i3_block = {
            "name": name,
            "full_text" : text
            }
    if color is not None: i3_block['color'] = color
    if bg is not None: i3_block['background'] = background
    if border is not None: i3_block['border'] = border
    if min_width is not None: i3_block['min_width'] = min_width
    if align is not None: i3_block['align'] = align
    if urgent is not None: i3_block['urgent'] = urgent
    if instance is not None: i3_block['instance'] = instance
    if separator is not None: i3_block['separator'] = separator
    if separator_block_width is not None: i3_block['separator_block_width'] = separator_block_width
    return i3_block

def plugged():
    acpi_output = check_output(["acpi", "-a"]).decode().replace('\n', '')
    plugged_in = "on" in acpi_output
    text = "" if plugged_in else ""
    return i3_json("plugged", text, color=nord_colors["nord13"], separator=False)

def battery():
    raw_acpi_output = check_output(["acpi", "-b"]).decode()
    acpi_output = raw_acpi_output.replace('\n', '').partition('%')[0]
    battery = int(acpi_output.split(' ')[-1])
    text = " "
    color = nord_colors["nord8"]
    if battery < 90:
        text = " "
        color = "#95da4c"
    elif battery < 60:
        text = " "
        color = "#27ae60"
    elif battery < 40:
        text = " "
        color = "#fdbc4b"
    elif battery < 20:
        text = " "
        color = "#f67400"
    elif battery < 10:
        text = "! "
        color = "#f44f4f"
    return i3_json("battery", text, color=color)


def xkb_layout():
    output = check_output(["xkb-switch"]).decode()
    text = output.replace('\n', '').partition('(')[0]
    return i3_json("xkblayout", text)

def clock():
    text = strftime("%Y-%m-%d %H:%M", localtime())
    return i3_json("clock", text)

def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(message + '\n')
    sys.stdout.flush()

def append_json(line, text):
    try:
        line.append(text)
    except:
        pass

if __name__ == '__main__':
    # Skip the first line which contains the version header.
    prefix = ','
    print_line('{ "version": 1 }')

    # The second line contains the start of the infinite array.
    print_line('[')

    # The third line prints and empty array so that we can always
    # print lines starting with commas afterward
    print_line('[]')

    while True:
        modules = [xkb_layout(), plugged(), battery(), clock()]
        line = []
        # insert information into the start of the json, but could be anywhere
        # CHANGE THIS LINE TO INSERT SOMETHING ELSE
        for mod in modules:
            append_json(line, mod)
        # and echo back new encoded json
        print_line(prefix+json.dumps(line))
        time.sleep(0.1)
