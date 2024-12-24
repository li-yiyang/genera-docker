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
    telnet telnetd \
    && rm -rf /var/lib/apt/lists/*

COPY ./dockerfiles/inetd.conf        /etc/inetd.conf
COPY ./dockerfiles/exports           /etc/exports
COPY ./dockerfiles/nfs-kernel-server /etc/default/nfs-kernel-server

VOLUME [ "/var/lib/symbolics" ]
WORKDIR /var/lib/symbolics
COPY ./symbolics/.VLM                /var/lib/symbolics/.VLM
COPY ./symbolics/VLM_debugger        /var/lib/symbolics/VLM_debugger
COPY ./symbolics/World.vlod          /var/lib/symbolics/World.vlod
COPY ./symbolics/genera              /var/lib/symbolics/genera
ADD  ./symbolics/sys.sct             /var/lib/symbolics/sys.sct

RUN mkdir LISP-MACHINE && touch LISP-MACHINE/lispm-init.lisp
RUN mkdir rel-8-6 && ln -s /var/lib/symbolics/sys.sct rel-8-6/sys.sct
RUN chmod -R 777 /var/lib/symbolics/sys.sct/*
RUN chmod -R 777 /var/lib/symbolics/LISP-MACHINE/*

# telnetd should set root with a password for Terminal login
RUN echo 'root:genera' | chpasswd

COPY ./dockerfiles/entrypoint.sh     /bin/entrypoint.sh
RUN  chmod a+x /bin/entrypoint.sh

CMD ["/bin/entrypoint.sh"]
