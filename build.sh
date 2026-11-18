#!/bin/sh

rm -r .org-cache/
rm -r public/
emacs -Q --script publish.el --funcall dw/publish

