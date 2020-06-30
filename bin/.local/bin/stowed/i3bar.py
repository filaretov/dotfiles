#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2019 Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
import sys
import json
import time
import subprocess
from pathlib import Path
from glob import glob
from time import localtime, strftime


def i3_json(name, text, **args):
    i3_block = {**{"name": name, "full_text": text}, **args}
    return i3_block


def ssid():
    rv = subprocess.check_output(["iwgetid", "-r"]).decode().strip()
    return i3_json("ssid", f"wifi: {rv}")


def current_task():
    try:
        with open(Path.home().joinpath(".current_task")) as f:
            task_name = f"{f.read().strip()}"
    except:
        task_name = ""
    return i3_json("pom", f"{task_name} ", color="#a4be8c")


def repo_is_dirty(directory):
    if ".git" in os.listdir(directory):
        try:
            rv = subprocess.check_output(
                ["git", "diff", "@{u}.."], cwd=directory, stderr=subprocess.STDOUT
            ).decode()
            rv += subprocess.check_output(
                ["git", "status", "--porcelain"], cwd=directory, stderr=subprocess.STDOUT
            ).decode()
        except subprocess.CalledProcessError:
            rv = ""
        return rv
    return False


def vsc_check():
    # No unpushed changes if output is empty
    git_repos = Path.home().joinpath("dev").iterdir()
    dirty = any([repo_is_dirty(dir) for dir in git_repos])
    text = "X " if dirty else "OK"
    symbol = "vsc:"
    return i3_json("vsc_check", f"{symbol} {text}")


def _plugged():
    acpi_output = subprocess.check_output(["acpi", "-a"]).decode().replace("\n", "")
    return "on" in acpi_output


def _battery_level():
    # Some laptops have two batteries (t460s, I'm looking at you)
    batteries = subprocess.check_output(["acpi", "-b"]).decode().split("\n")[:-1]
    left_output = [x.partition("%")[0] for x in batteries]
    battery_level = [int(x.split(" ")[-1]) for x in left_output]
    avg = sum(battery_level) / len(battery_level)
    return int(avg)


def battery():
    plugged = _plugged()
    battery_level = _battery_level()
    if plugged:
        symbol = "chr"
    else:
        symbol = "bat"
    text = f"{symbol}   {battery_level}"
    return i3_json("battery_level", text)


def xkb_layout():
    output = subprocess.check_output(["xkb-switch"]).decode()
    lang = output.replace("\n", "").partition("(")[0]
    text = f"kbd: {lang}"
    return i3_json("xkblayout", text)


def clock():
    date = strftime("%d/%m/%y %a %H:%M", localtime())
    symbol = ""
    text = f"{symbol}  {date}"
    return i3_json("clock", text, separator=False)


def brightness():
    try:
        value = float(subprocess.check_output(["brightnessctl", "get"]).decode())
        max = float(subprocess.check_output(["brightnessctl", "max"]).decode())
    except subprocess.CalledProcessError:
        value = -1.0
    value = value / max
    symbol = "scr"
    text = f"{symbol}  {value:.0%}"
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

    modules = [current_task, ssid, brightness, xkb_layout, battery, clock]
    while True:
        line = [attempt(mod) for mod in modules]
        print_line(prefix + json.dumps(line))
        time.sleep(1)
