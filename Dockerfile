# syntax=docker/dockerfile:1
FROM debian:bullseye-slim

ENV DISPLAY=host.docker.internal:0

# download dependencies
RUN apt-get update && apt-get install -y \
    inetutils-inetd \
    nfs-common \
    nfs-kernel-server \
    libx11-6 \
    libx11-xcb1 \
    iproute2 \
    systemd \
    && rm -rf /var/lib/apt/lists/*

COPY ./dockerfiles/inetd.conf        /etc/inetd.conf
COPY ./dockerfiles/exports           /etc/exports
COPY ./dockerfiles/nfs-kernel-server /etc/default/nfs-kernel-server

COPY ./dockerfiles/entrypoint.sh  /bin/entrypoint.sh
RUN  chmod a+x /bin/entrypoint.sh

CMD ["/bin/entrypoint.sh"]
