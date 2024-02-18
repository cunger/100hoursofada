# Stage 1: Install Alire

FROM ubuntu:22.04 AS alire

RUN apt-get update && apt-get install unzip -y

ADD https://github.com/alire-project/alire/releases/download/v1.2.1/alr-1.2.1-bin-x86_64-linux.zip ./
RUN unzip alr-1.2.1-bin-x86_64-linux.zip -d ./alire

# Stage 2: Build and run protohackers

FROM ubuntu:22.04

RUN apt-get update && apt-get install curl gcc git -y

COPY --from=alire alire ./alire
RUN ./alire/bin/alr -n toolchain --select

WORKDIR protohackers
COPY . .

RUN ../alire/bin/alr build

EXPOSE 7000/tcp

CMD ./bin/main
