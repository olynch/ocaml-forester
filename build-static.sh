#!/usr/bin/env bash

mkdir -p docker-build
docker build --output=docker-build --target=forester-built .

# TODO: figure out how to automate getting the release version here
tar -czf forester-4.2.0-prerelease-x86_64-unknown-linux-musl.tar.gz -C docker-build/bin forester
