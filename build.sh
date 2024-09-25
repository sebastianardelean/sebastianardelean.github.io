#!/bin/sh

emacs -Q --script publish.el --funcall dw/publish
emacs -Q --script blog-publish.el
