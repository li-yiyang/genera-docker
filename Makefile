# Reference:
# + https://archives.loomcom.com/genera/genera-install.html
# + https://gist.github.com/oubiwann/1e7aadfc22e3ae908921aeaccf27e82d
# + https://cliki.net/VLM_on_Linux
TOP  = $(shell pwd)
IMG  = genera
VOL  = symbolics

.PHONY: build

# See Dockerfile for building
# Keynotes:
# + refer to https://github.com/AkihiroSuda/containerized-systemd
#   for using systemd to setup NFS server and inetd
# + see dockerfiles/entrypoint.sh for commands
#   (especially, =cat >/etc/genera-cmd <<EOF= part)
build:
	docker build --platform linux/x86_64 -t $(IMG) .

# Run Genera
# xhost +127.0.0.1
run:
	docker run -dt \
		--name $(IMG) \
		--privileged=true \
		--platform linux/x86_64 \
		--hostname genera-host \
		--add-host genera-host:10.0.0.1 \
		--add-host genera:10.0.0.2 \
		--rm $(IMG)

# Attach to bash for debug usage
# or you can in Open Genera use Terminal (telnet)
# for interacting with docker debian system
bash:
	docker exec -it $(IMG) bash

# Stop Genera (Force)
stop:
	docker stop $(IMG)
