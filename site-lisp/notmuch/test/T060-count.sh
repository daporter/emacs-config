#!/usr/bin/env bash
test_description='"notmuch count" for messages and threads'
. ./test-lib.sh

add_email_corpus

# Note: The 'wc -l' results below are wrapped in arithmetic evaluation
# $((...)) to strip whitespace. This is for portability, as 'wc -l'
# emits whitespace on some BSD variants.

test_begin_subtest "message count is the default for notmuch count"
test_expect_equal \
    "$((`notmuch search --output=messages '*' | wc -l`))" \
    "`notmuch count '*'`"

test_begin_subtest "message count with --output=messages"
test_expect_equal \
    "$((`notmuch search --output=messages '*' | wc -l`))" \
    "`notmuch count --output=messages '*'`"

test_begin_subtest "thread count with --output=threads"
test_expect_equal \
    "$((`notmuch search --output=threads '*' | wc -l`))" \
    "`notmuch count --output=threads '*'`"

test_begin_subtest "thread count is the default for notmuch search"
test_expect_equal \
    "$((`notmuch search '*' | wc -l`))" \
    "`notmuch count --output=threads '*'`"

test_begin_subtest "files count"
test_expect_equal \
    "$((`notmuch search --output=files '*' | wc -l`))" \
    "`notmuch count --output=files '*'`"

test_begin_subtest "files count for a duplicate message-id"
test_expect_equal \
    "2" \
    "`notmuch count --output=files id:20091117232137.GA7669@griffis1.net`"

test_begin_subtest "count with no matching messages"
test_expect_equal \
    "0" \
    "`notmuch count --output=messages from:cworth and not from:cworth`"

test_begin_subtest "count with no matching threads"
test_expect_equal \
    "0" \
    "`notmuch count --output=threads from:cworth and not from:cworth`"

test_begin_subtest "message count is the default for batch count"
notmuch count --batch >OUTPUT <<EOF

from:cworth
EOF
notmuch count --output=messages >EXPECTED
notmuch count --output=messages from:cworth >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "batch message count"
notmuch count --batch --output=messages >OUTPUT <<EOF
from:cworth

tag:inbox
EOF
notmuch count --output=messages from:cworth >EXPECTED
notmuch count --output=messages >>EXPECTED
notmuch count --output=messages tag:inbox >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "batch thread count"
notmuch count --batch --output=threads >OUTPUT <<EOF

from:cworth
from:cworth and not from:cworth
foo
EOF
notmuch count --output=threads >EXPECTED
notmuch count --output=threads from:cworth >>EXPECTED
notmuch count --output=threads from:cworth and not from:cworth >>EXPECTED
notmuch count --output=threads foo >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT

test_begin_subtest "batch message count with input file"
cat >INPUT <<EOF
from:cworth

tag:inbox
EOF
notmuch count --input=INPUT --output=messages >OUTPUT
notmuch count --output=messages from:cworth >EXPECTED
notmuch count --output=messages >>EXPECTED
notmuch count --output=messages tag:inbox >>EXPECTED
test_expect_equal_file EXPECTED OUTPUT


test_done
