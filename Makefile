# Reference:
# + https://archives.loomcom.com/genera/genera-install.html
# + https://gist.github.com/oubiwann/1e7aadfc22e3ae908921aeaccf27e82d
# + https://cliki.net/VLM_on_Linux
TOP  = $(shell pwd)

.PHONY: build

build:
	docker build --platform linux/x86_64 -t genera .

volume:
	docker volume create \
		--driver local \
		--opt type=nfs \
		--opt o=addr=

run:
	docker run -it --name genera \
		--privileged \
    -v $(TOP)/symbolics:/var/lib/symbolics \
		--platform linux/x86_64 \
		--hostname genera-host \
		--add-host genera-host:10.0.0.1 \
		--add-host genera:10.0.0.2 \
		--rm genera
