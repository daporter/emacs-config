#!/usr/bin/env bash

set -eu
#set -x # or enter bash -x ... on command line

if [ x"${BASH_VERSION-}" = x ]
then	echo
	echo "Please execute this script using 'bash' interpreter"
	echo
	exit 1
fi

set -o posix
set -o pipefail # bash feature

readonly DEFAULT_IFS="$IFS" # Note: In this particular case quotes are needed.

# Avoid locale-specific differences in output of executed commands
LANG=C LC_ALL=C; export LANG LC_ALL

readonly PV_FILE='bindings/python/notmuch/version.py'

# Using array here turned out to be unnecessarily complicated
emsgs=''
emsg_count=0
append_emsg ()
{
	emsg_count=$((emsg_count + 1))
	emsgs="${emsgs:+$emsgs\n}  $1"
}

for f in ./version debian/changelog NEWS "$PV_FILE"
do
	if   [ ! -f "$f" ]; then append_emsg "File '$f' is missing"
	elif [ ! -r "$f" ]; then append_emsg "File '$f' is unreadable"
	elif [ ! -s "$f" ]; then append_emsg "File '$f' is empty"
	fi
done

if [ -n "$emsgs" ]
then
	echo 'Release files problems; fix these and try again:'
	echo -e "$emsgs"
	exit 1
fi

if read VERSION
then
	if read rest
	then	echo "'version' file contains more than one line"
		exit 1
	fi
else
	echo "Reading './version' file failed (suprisingly!)"
	exit 1
fi < ./version

readonly VERSION

# In the rest of this file, tests collect list of errors to be fixed

verfail ()
{
	echo No.
	append_emsg "$@"
	append_emsg "  Please follow the instructions in RELEASING to choose a version"
}

echo -n "Checking that '$VERSION' is good with digits and periods... "
case $VERSION in
	*[!0-9.]*)
		verfail "'$VERSION' contains other characters than digits and periods" ;;
	.*)	verfail "'$VERSION' begins with a period" ;;
	*.)	verfail "'$VERSION' ends with a period" ;;
	*..*)	verfail "'$VERSION' contains two consecutive periods" ;;
	*.*)	echo Yes. ;;
	*)	verfail "'$VERSION' is a single number" ;;
esac

echo -n "Checking that LIBNOTMUCH version macros & variables match ... "
# lib/notmuch.h
LIBNOTMUCH_MAJOR_VERSION=broken
LIBNOTMUCH_MINOR_VERSION=broken
LIBNOTMUCH_MICRO_VERSION=broken
# lib/Makefile.local
LIBNOTMUCH_VERSION_MAJOR=borken
LIBNOTMUCH_VERSION_MINOR=borken
LIBNOTMUCH_VERSION_RELEASE=borken

eval `awk 'NF == 3 && $1 == "#define" && $2 ~ /^LIBNOTMUCH_[A-Z]+_VERSION$/ \
	&& $3 ~ /^[0-9]+$/ { print $2 "=" $3 }' lib/notmuch.h`

eval `awk 'NF == 3 && $1 ~ /^LIBNOTMUCH_VERSION_[A-Z]+$/ && $2 == "=" \
	&& $3 ~ /^[0-9]+$/ { print $1 "=" $3 }' lib/Makefile.local`


check_version_component ()
{
	eval local v1=\$LIBNOTMUCH_$1_VERSION
	eval local v2=\$LIBNOTMUCH_VERSION_$2
	if [ $v1 != $v2 ]
	then	append_emsg "LIBNOTMUCH_$1_VERSION ($v1) does not equal LIBNOTMUCH_VERSION_$2 ($v2)"
	fi
}

old_emsg_count=$emsg_count
check_version_component MAJOR MAJOR
check_version_component MINOR MINOR
check_version_component MICRO RELEASE
[ $old_emsg_count = $emsg_count ] && echo Yes. || echo No.

echo -n "Checking that this is Debian package for notmuch... "
read deb_notmuch deb_version rest < debian/changelog
if [ "$deb_notmuch" = 'notmuch' ]
then
	echo Yes.
else
	echo No.
	append_emsg "Package name '$deb_notmuch' is not 'notmuch' in debian/changelog"
fi

echo -n "Checking that Debian package version is $VERSION-1... "

if [ "$deb_version" = "($VERSION-1)" ]
then
	echo Yes.
else
	echo No.
	append_emsg "Version '$deb_version' is not '($VERSION-1)' in debian/changelog"
fi

echo -n "Checking that python bindings version is $VERSION... "
py_version=`python -c "with open('$PV_FILE') as vf: exec(vf.read()); print __VERSION__"`
if [ "$py_version" = "$VERSION" ]
then
	echo Yes.
else
	echo No.
	append_emsg "Version '$py_version' is not '$VERSION' in $PV_FILE"
fi

echo -n "Checking that NEWS header is tidy... "
if [ "`exec sed 's/./=/g; 1q' NEWS`" = "`exec sed '1d; 2q' NEWS`" ]
then
	echo Yes.
else
	echo No.
	if [ "`exec sed '1d; s/=//g; 2q' NEWS`" != '' ]
	then
		append_emsg "Line 2 in NEWS file is not all '=':s"
	else
		append_emsg "Line 2 in NEWS file does not have the same length as line 1"
	fi
fi

echo -n "Checking that this is Notmuch NEWS... "
read news_notmuch news_version news_date < NEWS
if [ "$news_notmuch" = "Notmuch" ]
then
	echo Yes.
else
	echo No.
	append_emsg "First word '$news_notmuch' is not 'Notmuch' in NEWS file"
fi

echo -n "Checking that NEWS version is $VERSION... "
if [ "$news_version" = "$VERSION" ]
then
	echo Yes.
else
	echo No.
	append_emsg "Version '$news_version' in NEWS file is not '$VERSION'"
fi

#eval `date '+year=%Y mon=%m day=%d'`
today0utc=`date --date=0Z +%s` # gnu date feature

echo -n "Checking that NEWS date is right... "
case $news_date in
 '('[2-9][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]')')
	newsdate0utc=`nd=${news_date#\\(}; date --date="${nd%)} 0Z" +%s`
	ddiff=$((newsdate0utc - today0utc))
	if [ $ddiff -lt -86400 ] # since beginning of yesterday...
	then
		echo No.
		append_emsg "Date $news_date in NEWS file is too much in the past"
	elif [ $ddiff -gt 172800 ] # up to end of tomorrow...
	then
		echo No.
		append_emsg "Date $news_date in NEWS file is too much in the future"
	else
		echo Yes.
	fi ;;
 *)
	echo No.
	append_emsg "Date '$news_date' in NEWS file is not in format (yyyy-mm-dd)"
esac

if [ -n "$emsgs" ]
then
	echo
	echo 'Release check failed; check these issues:'
	echo -e "$emsgs"
	exit 1
fi

echo 'All checks this script executed completed successfully.'
echo 'Make sure that everything else mentioned in RELEASING'
echo 'file is in order, too.'


# Local variables:
# mode: shell-script
# sh-basic-offset: 8
# tab-width: 8
# End:
# vi: set sw=8 ts=8
