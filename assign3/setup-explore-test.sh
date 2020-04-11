#!/bin/bash

usage() {
    local progname
    progname=$1
    echo "Usage: $progname <tmp-dir>" >&2
    echo "For example:" >&2
    echo "Usage: $progname spec-run-temp" >&2
}

if [ $1 == "-h" -o $1 == "--help" ]; then
    usage $0
    exit 0
fi

if [ $# != 1 ]; then
    usage $0
    exit 1
fi

tmpdir=$1
git="git -C $tmpdir --git-dir=.idiot --work-tree=."
file1=file1
file2=direct/file2

git_commit() {
    $git add $file1
    $git add $file2
    $git commit --no-gpg-sign -m "$1"
}

rm -rf $tmpdir
mkdir -p $tmpdir
export GIT_AUTHOR_NAME="Linus Torvalds"
export GIT_AUTHOR_EMAIL="torvalds@transmeta.com"
export GIT_AUTHOR_DATE="1581997446 -0500"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"
export GIT_COMMITTER_DATE="$GIT_AUTHOR_DATE"
$git init
mkdir $tmpdir/direct
echo Greta > $tmpdir/$file1
echo Jupiter > $tmpdir/$file2
git_commit "commit message"
$git switch -c feature-x
echo Adelaide > $tmpdir/$file2
git_commit "add feature X\n\nlotsa stuff got done here yo"
$git switch master

