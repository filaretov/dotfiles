#!/bin/bash
# SPDX-FileCopyrightText: Hristo Filaretov <h.filaretov@campus.tu-berlin.de>
# SPDX-License-Identifier: MIT
# xsel didn't work for some reason
pwgen 64 1 | xclip -sel clip
notify-send "Password copied to clipboard" -t 750
