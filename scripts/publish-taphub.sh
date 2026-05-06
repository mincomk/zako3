#!/usr/bin/env bash

mkdir dist
cp target/release/zako3-taphub-core dist
patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 ./dist/zako3-taphub-core
podman build -f services/taphub/Dockerfile -t ghcr.io/zako-ac/taphub:latest .
podman push ghcr.io/zako-ac/taphub:latest
