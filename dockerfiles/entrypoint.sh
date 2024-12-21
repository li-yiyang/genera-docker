#!/bin/sh

# ref: https://github.com/AkihiroSuda/containerized-systemd
set -e
container=docker
export container

env > /etc/genera-env

cat >/etc/systemd/system/genera.target <<EOF
[Unit]
Description=the target for genera.service
Requires=genera.service systemd-logind.service systemd-user-sessions.service
EOF

cat >/etc/genera-cmd <<EOF
/etc/init.d/nfs-kernel-server start
/etc/init.d/inetutils-inetd   start

ip tuntap add dev tap0 mode tap
ip addr add 10.0.0.1/24 dev tap0
ip link set dev tap0 up

chmod -R 777 /var/lib/symbolics/sys.sct/*
cd /var/lib/symbolics/ && ./genera
EOF

cat >/etc/systemd/system/genera.service <<EOF
[Unit]
Description=OpenGenera 2.0 in Docker service
#Requires=inetd.service nfs.service
#After=inetd.service nfs.service

[Service]
ExecStart=/bin/bash -exc "source /etc/genera-cmd"
ExecStopPost=/bin/bash -ec "if echo \${EXIT_STATUS} | grep [A-Z] > /dev/null; then echo >&2 \"got signal \${EXIT_STATUS}\"; systemctl exit \$(( 128 + \$( kill -l \${EXIT_STATUS} ) )); else systemctl exit \${EXIT_STATUS}; fi"
StandardInput=tty-force
StandardOutput=inherit
StandardError=inherit
WorkingDirectory=/var/lib/symbolics
EnvironmentFile=/etc/genera-env

[Install]
WantedBy=multi-user.target
EOF

systemctl mask systemd-firstboot.service systemd-udevd.service systemd-modules-load.service
systemctl unmask systemd-logind
systemctl enable genera.service

systemd=/lib/systemd/systemd

systemd_args="--unit=genera.target"
echo "$0: starting $systemd $systemd_args"
exec $systemd $systemd_args
