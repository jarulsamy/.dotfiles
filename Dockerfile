FROM debian:latest

RUN apt-get -y update && \
    apt-get install -y --no-install-recommends sudo git youtube-dl ffmpeg

RUN useradd -m admin && echo "admin:admin" | chpasswd && adduser admin sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER admin

RUN git clone git://github.com/jarulsamy/.dotfiles /home/admin/.dotfiles

WORKDIR /home/admin/.dotfiles

RUN ./install.sh && ./setup.sh

ENTRYPOINT [ "/bin/zsh" ]
