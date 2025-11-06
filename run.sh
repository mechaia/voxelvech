#!/bin/sh

SSIL_TO_PIL="python3 data/transpile_ssil_to_pil.py"

set -xe

$SSIL_TO_PIL data/scenarios/tutorial/eviction.ssil > data/scenarios/tutorial/eviction.pil
$SSIL_TO_PIL data/scenarios/tutorial/ai.ssil > data/scenarios/tutorial/ai.pil

cargo r -- scenario "$@"
