#!/bin/bash

OPA_VSN="0.23.2-debug"
OPA_IMAGE="openpolicyagent/opa:${OPA_VSN}"

POLICY_DIR="/var/opa/policies"

cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      opa:
        condition: service_healthy

  opa:
    image: ${OPA_IMAGE}
    entrypoint: ["sh", "-c"]
    command: ["cd ${POLICY_DIR} &&
               /opa build . &&
               /opa run
                 --server
                 --addr :8181
                 --set decision_logs.console=true
                 --bundle bundle.tar.gz
               "]
    volumes:
      - ./test/policies:${POLICY_DIR}:rw
    healthcheck:
      test: ["CMD", "sh", "-c", "wget -q --tries=1 --spider http://localhost:8181/"]
      interval: 5s
      timeout: 1s
      retries: 5
EOF
