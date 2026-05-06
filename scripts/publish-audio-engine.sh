#!/usr/bin/env bash

mkdir dist
cp target/release/zako3-audio-engine-controller dist
patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 ./dist/zako3-audio-engine-controller
podman build -f services/audio-engine/Dockerfile -t ghcr.io/zako-ac/audio-engine:latest .
podman push ghcr.io/zako-ac/audio-engine:latest
