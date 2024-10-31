#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 The Forester Project Contributors
#
# SPDX-License-Identifier: GPL-3.0-or-later

mkdir -p docker-build
docker buildx build . --output=forester

# TODO: figure out how to automate getting the release version here
tar -czf forester-5.0-dev-x86_64-unknown-linux-musl.tar.gz forester
