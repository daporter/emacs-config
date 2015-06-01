#!/usr/bin/env bash
test_description='"notmuch address" in several variants'
. ./test-lib.sh

add_email_corpus

test_begin_subtest "--output=sender"
notmuch address --output=sender '*' >OUTPUT
cat <<EOF >EXPECTED
François Boulogne <boulogne.f@gmail.com>
Olivier Berger <olivier.berger@it-sudparis.eu>
Chris Wilson <chris@chris-wilson.co.uk>
Carl Worth <cworth@cworth.org>
Alexander Botero-Lowry <alex.boterolowry@gmail.com>
Keith Packard <keithp@keithp.com>
Jjgod Jiang <gzjjgod@gmail.com>
Rolland Santimano <rollandsantimano@yahoo.com>
Jan Janak <jan@ryngle.com>
Stewart Smith <stewart@flamingspork.com>
Lars Kellogg-Stedman <lars@seas.harvard.edu>
Alex Botero-Lowry <alex.boterolowry@gmail.com>
Ingmar Vanhassel <ingmar@exherbo.org>
Aron Griffis <agriffis@n01se.net>
Adrian Perez de Castro <aperez@igalia.com>
Israel Herraiz <isra@herraiz.org>
Mikhail Gusarov <dottedmag@dottedmag.net>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "without --output"
notmuch address '*' >OUTPUT
# Use EXPECTED from previous subtest
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=sender --format=json"
notmuch address --output=sender --format=json '*' >OUTPUT
cat <<EOF >EXPECTED
[{"name": "François Boulogne", "address": "boulogne.f@gmail.com", "name-addr": "François Boulogne <boulogne.f@gmail.com>"},
{"name": "Olivier Berger", "address": "olivier.berger@it-sudparis.eu", "name-addr": "Olivier Berger <olivier.berger@it-sudparis.eu>"},
{"name": "Chris Wilson", "address": "chris@chris-wilson.co.uk", "name-addr": "Chris Wilson <chris@chris-wilson.co.uk>"},
{"name": "Carl Worth", "address": "cworth@cworth.org", "name-addr": "Carl Worth <cworth@cworth.org>"},
{"name": "Alexander Botero-Lowry", "address": "alex.boterolowry@gmail.com", "name-addr": "Alexander Botero-Lowry <alex.boterolowry@gmail.com>"},
{"name": "Keith Packard", "address": "keithp@keithp.com", "name-addr": "Keith Packard <keithp@keithp.com>"},
{"name": "Jjgod Jiang", "address": "gzjjgod@gmail.com", "name-addr": "Jjgod Jiang <gzjjgod@gmail.com>"},
{"name": "Rolland Santimano", "address": "rollandsantimano@yahoo.com", "name-addr": "Rolland Santimano <rollandsantimano@yahoo.com>"},
{"name": "Jan Janak", "address": "jan@ryngle.com", "name-addr": "Jan Janak <jan@ryngle.com>"},
{"name": "Stewart Smith", "address": "stewart@flamingspork.com", "name-addr": "Stewart Smith <stewart@flamingspork.com>"},
{"name": "Lars Kellogg-Stedman", "address": "lars@seas.harvard.edu", "name-addr": "Lars Kellogg-Stedman <lars@seas.harvard.edu>"},
{"name": "Alex Botero-Lowry", "address": "alex.boterolowry@gmail.com", "name-addr": "Alex Botero-Lowry <alex.boterolowry@gmail.com>"},
{"name": "Ingmar Vanhassel", "address": "ingmar@exherbo.org", "name-addr": "Ingmar Vanhassel <ingmar@exherbo.org>"},
{"name": "Aron Griffis", "address": "agriffis@n01se.net", "name-addr": "Aron Griffis <agriffis@n01se.net>"},
{"name": "Adrian Perez de Castro", "address": "aperez@igalia.com", "name-addr": "Adrian Perez de Castro <aperez@igalia.com>"},
{"name": "Israel Herraiz", "address": "isra@herraiz.org", "name-addr": "Israel Herraiz <isra@herraiz.org>"},
{"name": "Mikhail Gusarov", "address": "dottedmag@dottedmag.net", "name-addr": "Mikhail Gusarov <dottedmag@dottedmag.net>"}]
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=recipients"
notmuch address --output=recipients '*' >OUTPUT
cat <<EOF >EXPECTED
Allan McRae <allan@archlinux.org>
"Discussion about the Arch User Repository (AUR)" <aur-general@archlinux.org>
olivier.berger@it-sudparis.eu
notmuch@notmuchmail.org
notmuch <notmuch@notmuchmail.org>
Keith Packard <keithp@keithp.com>
Mikhail Gusarov <dottedmag@dottedmag.net>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=sender --output=recipients"
notmuch address --output=sender --output=recipients '*' >OUTPUT
cat <<EOF >EXPECTED
François Boulogne <boulogne.f@gmail.com>
Allan McRae <allan@archlinux.org>
"Discussion about the Arch User Repository (AUR)" <aur-general@archlinux.org>
Olivier Berger <olivier.berger@it-sudparis.eu>
olivier.berger@it-sudparis.eu
Chris Wilson <chris@chris-wilson.co.uk>
notmuch@notmuchmail.org
Carl Worth <cworth@cworth.org>
Alexander Botero-Lowry <alex.boterolowry@gmail.com>
Keith Packard <keithp@keithp.com>
Jjgod Jiang <gzjjgod@gmail.com>
Rolland Santimano <rollandsantimano@yahoo.com>
Jan Janak <jan@ryngle.com>
Stewart Smith <stewart@flamingspork.com>
Lars Kellogg-Stedman <lars@seas.harvard.edu>
notmuch <notmuch@notmuchmail.org>
Alex Botero-Lowry <alex.boterolowry@gmail.com>
Ingmar Vanhassel <ingmar@exherbo.org>
Aron Griffis <agriffis@n01se.net>
Adrian Perez de Castro <aperez@igalia.com>
Israel Herraiz <isra@herraiz.org>
Mikhail Gusarov <dottedmag@dottedmag.net>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=sender --output=count"
notmuch address --output=sender --output=count '*' | sort -n >OUTPUT
cat <<EOF >EXPECTED
1	Adrian Perez de Castro <aperez@igalia.com>
1	Aron Griffis <agriffis@n01se.net>
1	Chris Wilson <chris@chris-wilson.co.uk>
1	François Boulogne <boulogne.f@gmail.com>
1	Ingmar Vanhassel <ingmar@exherbo.org>
1	Israel Herraiz <isra@herraiz.org>
1	Olivier Berger <olivier.berger@it-sudparis.eu>
1	Rolland Santimano <rollandsantimano@yahoo.com>
2	Alex Botero-Lowry <alex.boterolowry@gmail.com>
2	Jjgod Jiang <gzjjgod@gmail.com>
3	Stewart Smith <stewart@flamingspork.com>
4	Alexander Botero-Lowry <alex.boterolowry@gmail.com>
4	Jan Janak <jan@ryngle.com>
5	Lars Kellogg-Stedman <lars@seas.harvard.edu>
5	Mikhail Gusarov <dottedmag@dottedmag.net>
7	Keith Packard <keithp@keithp.com>
12	Carl Worth <cworth@cworth.org>
EOF
test_expect_equal_file OUTPUT EXPECTED

test_begin_subtest "--output=count --format=json"
# Since the iteration order of GHashTable is not specified, we
# preprocess and sort the results to keep the order stable here.
notmuch address --output=count --format=json '*' | \
    sed -e 's/^\[//' -e 's/]$//' -e 's/,$//' | sort >OUTPUT
cat <<EOF >EXPECTED
{"name": "Adrian Perez de Castro", "address": "aperez@igalia.com", "name-addr": "Adrian Perez de Castro <aperez@igalia.com>", "count": 1}
{"name": "Alex Botero-Lowry", "address": "alex.boterolowry@gmail.com", "name-addr": "Alex Botero-Lowry <alex.boterolowry@gmail.com>", "count": 2}
{"name": "Alexander Botero-Lowry", "address": "alex.boterolowry@gmail.com", "name-addr": "Alexander Botero-Lowry <alex.boterolowry@gmail.com>", "count": 4}
{"name": "Aron Griffis", "address": "agriffis@n01se.net", "name-addr": "Aron Griffis <agriffis@n01se.net>", "count": 1}
{"name": "Carl Worth", "address": "cworth@cworth.org", "name-addr": "Carl Worth <cworth@cworth.org>", "count": 12}
{"name": "Chris Wilson", "address": "chris@chris-wilson.co.uk", "name-addr": "Chris Wilson <chris@chris-wilson.co.uk>", "count": 1}
{"name": "François Boulogne", "address": "boulogne.f@gmail.com", "name-addr": "François Boulogne <boulogne.f@gmail.com>", "count": 1}
{"name": "Ingmar Vanhassel", "address": "ingmar@exherbo.org", "name-addr": "Ingmar Vanhassel <ingmar@exherbo.org>", "count": 1}
{"name": "Israel Herraiz", "address": "isra@herraiz.org", "name-addr": "Israel Herraiz <isra@herraiz.org>", "count": 1}
{"name": "Jan Janak", "address": "jan@ryngle.com", "name-addr": "Jan Janak <jan@ryngle.com>", "count": 4}
{"name": "Jjgod Jiang", "address": "gzjjgod@gmail.com", "name-addr": "Jjgod Jiang <gzjjgod@gmail.com>", "count": 2}
{"name": "Keith Packard", "address": "keithp@keithp.com", "name-addr": "Keith Packard <keithp@keithp.com>", "count": 7}
{"name": "Lars Kellogg-Stedman", "address": "lars@seas.harvard.edu", "name-addr": "Lars Kellogg-Stedman <lars@seas.harvard.edu>", "count": 5}
{"name": "Mikhail Gusarov", "address": "dottedmag@dottedmag.net", "name-addr": "Mikhail Gusarov <dottedmag@dottedmag.net>", "count": 5}
{"name": "Olivier Berger", "address": "olivier.berger@it-sudparis.eu", "name-addr": "Olivier Berger <olivier.berger@it-sudparis.eu>", "count": 1}
{"name": "Rolland Santimano", "address": "rollandsantimano@yahoo.com", "name-addr": "Rolland Santimano <rollandsantimano@yahoo.com>", "count": 1}
{"name": "Stewart Smith", "address": "stewart@flamingspork.com", "name-addr": "Stewart Smith <stewart@flamingspork.com>", "count": 3}
EOF
test_expect_equal_file OUTPUT EXPECTED

test_done
