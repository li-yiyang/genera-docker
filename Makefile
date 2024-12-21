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

# prepare all files under symbolics:
# + .VLM file
# + *.vlod
# + VLM_debugger
# + genera
# + sys.sct
volume:
	docker volume create $(VOL)
	docker run -it \
		--platform linux/x86_64 \
		-v $(VOL):/var/lib/symbolics \
		-v $(TOP)/symbolics:/symbolics \
		--rm $(IMG) \
		bash -c "cp -r /symbolics/{.VLM,*.vlod,VLM_debugger,genera,sys.sct} /var/lib/symbolics"

# copy files into volume
# + .VLM file
update_VLM:
	docker run -it \
		--platform linux/x86_64 \
		-v $(VOL):/var/lib/symbolics \
		-v $(TOP)/symbolics:/symbolics \
		--rm $(IMG) \
		bash -c "cp /symbolics/{.VLM} /var/lib/symbolics"

# Run Genera
run:
	# xhost +127.0.0.1
	docker run -dt \
		--name $(IMG) \
		--privileged=true \
		-v $(VOL):/var/lib/symbolics \
		--platform linux/x86_64 \
		--hostname genera-host \
		--add-host genera-host:10.0.0.1 \
		--add-host genera:10.0.0.2 \
		--rm $(IMG)

# Attach to bash for debug usage
bash:
	docker exec -it $(IMG) bash

# Stop Genera
stop:
	docker stop $(IMG)
