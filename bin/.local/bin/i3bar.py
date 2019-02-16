#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import json
import time
import argparse
import functools
from subprocess import check_output
from time import localtime, strftime

nord_colors = {
    "nord0": "#2E3440",
    "nord1": "#3B4252",
    "nord2": "#434C5E",
    "nord3": "#4C566A",
    "nord4": "#D8DEE9",
    "nord5": "#E5E9F0",
    "nord6": "#ECEFF4",
    "nord7": "#8FBCBB",
    "nord8": "#88C0D0",
    "nord9": "#81A1C1",
    "nord10": "#5E81AC",
    "nord11": "#BF616A",
    "nord12": "#D08770",
    "nord13": "#EBCB8B",
    "nord14": "#A3BE8C",
    "nord15": "#B48EAD",
}

bottom = 2


def i3_json(
    name,
    text,
    color=None,
    bg=None,
    border=None,
    min_width=None,
    align=None,
    urgent=None,
    instance=None,
    separator=None,
    separator_block_width=None,
):
    i3_block = {"name": name, "full_text": text}
    if color is not None:
        i3_block["color"] = color
    if bg is not None:
        i3_block["background"] = bg
    if border is not None:
        i3_block["border"] = border
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
    return i3_json("vsc_check", " id", separator=True, bg=nord_colors["nord10"])


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
    color = nord_colors["nord8"]
    if plugged:
        text = ""
        color = nord_colors["nord13"]
    elif battery_level < 90:
        text = " "
        color = "#95da4c"
    elif battery_level < 60:
        text = " "
        color = "#27ae60"
    elif battery_level < 40:
        text = " "
        color = "#fdbc4b"
    elif battery_level < 20:
        text = " "
        color = "#f67400"
    elif battery_level < 10:
        text = "! "
        color = "#f44f4f"
    text = text + " " + str(battery_level)
    return i3_json("battery_level", text, bg=color)


def xkb_layout():
    output = check_output(["xkb-switch"]).decode()
    text = output.replace("\n", "").partition("(")[0]
    text = " " + text
    return i3_json("xkblayout", text, bg=nord_colors["nord15"])


def clock():
    date, time = strftime("%Y-%m-%d %H:%M", localtime()).split(" ")
    text = " " + date + "  " + time
    return i3_json("clock", text, bg=nord_colors["nord7"])


def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(message + "\n")
    sys.stdout.flush()


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--noborders", help="Disable border entries for all items", action="store_true"
    )
    parser.add_argument(
        "--nobackground", help="Disable background entries for all items", action="store_true"
    )
    return parser.parse_args()


def get_filter(key):
    def filter(dictionary):
        return {k:v for (k,v) in dictionary.items() if k != key}
    return filter


def compose(*functions):
    return functools.reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)


if __name__ == "__main__":
    # Skip the first line which contains the version header.
    args = parse_args()
    filters = []
    if args.noborders:
        filters.append(get_filter("border"))
    if args.nobackground:
        filters.append(get_filter("background"))
    filt = compose(*filters)
    prefix = ","
    print_line('{ "version": 1 }')

    # The second line contains the start of the infinite array.
    print_line("[")

    # The third line prints and empty array so that we can always
    # print lines starting with commas afterward
    print_line("[]")

    modules = [xkb_layout, battery, clock]
    while True:
        line = [filt(mod()) for mod in modules]
        print_line(prefix + json.dumps(line))
        time.sleep(0.1)
