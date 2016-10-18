#!/bin/sh

git status
git checkout sync
git status
git merge master
git push --follow-tags -u bb sync
git checkout master
git status
