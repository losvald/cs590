#!/bin/sh
tar zcvf "$1-leo_osvald.tar.gz" "$1" --exclude=project --exclude=target --exclude='.*'
