#!/bin/bash

set -e

# Get type names from .erl
grep "<<\"normalize"  normalize_piqi.erl | sed "s/.*<<\(.*\)>>.*/\1/" > normalize-piqi-typenames
grep "<<\"nonormalize"  nonormalize_piqi.erl | sed "s/.*<<\(.*\)>>.*/\1/" > nonormalize-piqi-typenames

# These should be the same
cmp normalize.piq normalize_piqi_from_erl.piq
cmp nonormalize.piq nonormalize_piqi_from_erl.piq

# Generated hrl should be equal
cmp normalize_piqi.hrl normalize_piqi.hrl-test
cmp nonormalize_piqi.hrl nonormalize_piqi.hrl-test

# Check if typenames are equal
cmp normalize-piqi-typenames normalize-piqi-typenames-test
cmp nonormalize-piqi-typenames nonormalize-piqi-typenames-test

# Check conversions
escript normalize_piqi_test.beam test_convert_normalize
escript normalize_piqi_test.beam test_convert_nonormalize
