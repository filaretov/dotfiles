#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
import sys
import json
import time
import os
import subprocess
from glob import glob
from time import localtime, strftime

HOME = os.path.expanduser("~")

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

def pom_task():
    try:
        with open("/tmp/pom_task") as f:
            task_name = f.read().strip()
    except:
        task_name = ""
    return i3_json("pom", task_name)

def repo_is_dirty(dir):
    if ".git" in os.listdir(dir):
        try:
            rv = subprocess.check_output(
                ["git", "diff", "@{u}.."], cwd=dir, stderr=subprocess.STDOUT
            ).decode()
            rv += subprocess.check_output(
                ["git", "status", "--porcelain"], cwd=dir, stderr=subprocess.STDOUT
            ).decode()
        except subprocess.CalledProcessError:
            rv = ""
        return rv
    else:
        return False


def vsc_check():
    # No unpushed changes if output is empty
    git_repos = glob(HOME + "/dev/*/")
    dirty = any([repo_is_dirty(dir) for dir in git_repos])
    text = "X " if dirty else "OK"
    return i3_json("vsc_check", f"repos: {text}")


def _plugged():
    acpi_output = subprocess.check_output(["acpi", "-a"]).decode().replace("\n", "")
    return "on" in acpi_output


def _battery_level():
    # Some laptops have two batteries (t460s, I'm looking at you)
    batteries = subprocess.check_output(["acpi", "-b"]).decode().split("\n")[:-1]
    left_output = [x.partition("%")[0] for x in batteries]
    battery_level = [int(x.split(" ")[-1]) for x in left_output]
    avg = sum(battery_level)/len(battery_level)
    return int(avg)


def battery():
    plugged = _plugged()
    battery_level = _battery_level()
    if plugged:
        symbol = "chr"
    else:
        symbol = "bat"
    text = f"{symbol}: {battery_level}"
    return i3_json("battery_level", text)


def xkb_layout():
    output = subprocess.check_output(["xkb-switch"]).decode()
    lang = output.replace("\n", "").partition("(")[0]
    text = f"kbd: {lang}"
    return i3_json("xkblayout", text)


def clock():
    text = strftime("%D %a %H:%M", localtime())
    return i3_json("clock", text, separator=False)


def brightness():
    try:
        value = float(subprocess.check_output(["xbacklight"]).decode())
    except subprocess.CalledProcessError:
        value = -1.0
    text = f"scrn: {value:3.0f}"
    return i3_json("brightness", text)


def print_line(message):
    """ Non-buffered printing to stdout. """
    sys.stdout.write(f"{message}\n")
    sys.stdout.flush()


def empty():
    return i3_json("nil", "")


def attempt(func):
    try:
        return func()
    except:
        return empty()


if __name__ == "__main__":
    # Skip the first line which contains the version header.
    prefix = ","
    print_line('{ "version": 1 }')

    # The second line contains the start of the infinite array.
    print_line("[")

    # The third line prints and empty array so that we can always
    # print lines starting with commas afterward
    print_line("[]")

    modules = [brightness, xkb_layout, vsc_check, battery, clock]
    while True:
        line = [attempt(mod) for mod in modules]
        print_line(prefix + json.dumps(line))
        time.sleep(1)
