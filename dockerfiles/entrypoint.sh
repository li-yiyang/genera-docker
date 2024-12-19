#!/bin/sh

/etc/init.d/nfs-kernel-server start
/etc/init.d/inetutils-inetd   start

ip tuntap add dev tap0 mode tap
ip addr add 10.0.0.1/24 dev tap0
ip link set dev tap0 up

cd /var/lib/symbolics && genera
