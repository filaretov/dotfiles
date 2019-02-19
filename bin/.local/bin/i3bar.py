#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import json
import time
from subprocess import check_output
from time import localtime, strftime

def i3_json(
    name,
    text,
    min_width=None,
    align=None,
    urgent=None,
    instance=None,
    separator=None,
    separator_block_width=None,
):
    i3_block = {"name": name, "full_text": text}
    if min_width is not None:
        i3_block["min_width"] = min_width
    if align is not None:
        i3_block["align"] = align
    if urgent is not None:
        i3_block["urgent"] = urgent
    if instance is not None:
        i3_block["instance"] = instance
    if separator is not None:
        i3_block["separator"] = separator
    if separator_block_width is not None:
        i3_block["separator_block_width"] = separator_block_width
    return i3_block


def vsc_check():
    return i3_json("vsc_check", " id", separator=True)


def _plugged():
    acpi_output = check_output(["acpi", "-a"]).decode().replace("\n", "")
    return "on" in acpi_output


def _battery_level():
    raw_acpi_output = check_output(["acpi", "-b"]).decode().replace("\n", "")
    acpi_output = raw_acpi_output.partition("%")[0]
    battery_level = int(acpi_output.split(" ")[-1])
    return battery_level


def battery():
    plugged = _plugged()
    battery_level = _battery_level()
    text = " "
    if plugged:
        text = ""
    elif battery_level < 90:
        text = " "
    elif battery_level < 60:
        text = " "
    elif battery_level < 40:
        text = " "
    elif battery_level < 20:
        text = " "
    elif battery_level < 10:
        text = "! "
    text = text + " " + str(battery_level)
    return i3_json("battery_level", text)


def xkb_layout():
    output = check_output(["xkb-switch"]).decode()
    text = output.replace("\n", "").partition("(")[0]
    text = " " + text
    return i3_json("xkblayout", text)


def clock():
    date, time = strftime("%Y-%m-%d %H:%M", localtime()).split(" ")
    text = " " + date + "  " + time
    return i3_json("clock", text)


def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(message + "\n")
    sys.stdout.flush()

if __name__ == "__main__":
    # Skip the first line which contains the version header.
    prefix = ","
    print_line('{ "version": 1 }')

    # The second line contains the start of the infinite array.
    print_line("[")

    # The third line prints and empty array so that we can always
    # print lines starting with commas afterward
    print_line("[]")

    modules = [xkb_layout, battery, clock]
    while True:
        line = [mod() for mod in modules]
        print_line(prefix + json.dumps(line))
        time.sleep(0.1)
