#!/usr/bin/env bash

test_description="online help"
. ./test-lib.sh

test_expect_success 'notmuch --help' 'notmuch --help'
test_expect_success 'notmuch help' 'notmuch help'
test_expect_success 'notmuch --version' 'notmuch --version'

if [ $NOTMUCH_HAVE_MAN -eq 1 ]; then
    test_expect_success 'notmuch --help tag' 'notmuch --help tag'
    test_expect_success 'notmuch help tag' 'notmuch help tag'
else
    test_expect_success 'notmuch --help tag (man pages not available)' \
	'test_must_fail notmuch --help tag >/dev/null'
    test_expect_success 'notmuch help tag (man pages not available)' \
	'test_must_fail notmuch help tag >/dev/null'
fi

test_done
