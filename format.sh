#!/bin/bash

# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
#
# SPDX-License-Identifier: GPL-3.0-or-later

export TOPIARY_LANGUAGE_DIR=topiary

topiary format bin/**/*.ml
topiary format lib/**/*.ml
topiary format lib/**/*.mli
topiary format lib/**/*.mll
