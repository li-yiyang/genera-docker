# syntax=docker/dockerfile:1
FROM debian:bullseye-slim

ENV TZ=Asia/Shanghai
ENV DISPLAY=host.docker.internal:0

# correct timezone
RUN apt-get update && apt-get install -y tzdata
RUN ln -sf /usr/share/zoneinfo/$TZ /etc/localtime
RUN dpkg-reconfigure -f noninteractive tzdata

# download dependencies
RUN apt-get install -y \
    inetutils-inetd \
    nfs-common \
    nfs-kernel-server \
    libx11-6 \
    libx11-xcb1 \
    iproute2 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /bin

COPY ./dockerfiles/inetd.conf     /etc/inetd.conf
COPY ./dockerfiles/exports        /etc/exports

COPY ./dockerfiles/VLM_debugger   ./VLM_debugger
COPY ./dockerfiles/genera         ./genera
COPY ./dockerfiles/entrypoint.sh  ./entrypoint.sh

RUN  chmod a+x genera
RUN  chmod a+x entrypoint.sh

ENTRYPOINT ["entrypoint.sh"]