#!/bin/bash

export TOPIARY_LANGUAGE_DIR=topiary

topiary format bin/**/*.ml
topiary format lib/**/*.ml
topiary format lib/**/*.mli
topiary format lib/**/*.mll
