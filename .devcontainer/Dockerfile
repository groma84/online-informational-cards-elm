FROM mcr.microsoft.com/vscode/devcontainers/javascript-node:0-14-buster

# Configuring versions
ARG ELM_VERSION=latest-0.19.1
ARG ELM_TEST_VERSION=latest-0.19.1
ARG ELM_FORMAT_VERSION=latest-0.19.1
ARG ELM_REVIEW_VERSION=2.5.1
ARG CREATE_ELM_APP_VERSION=5.22.0

# This Dockerfile adds a non-root user with sudo access. Update the “remoteUser” property in
# devcontainer.json to use it. More info: https://aka.ms/vscode-remote/containers/non-root-user.
ARG USERNAME=node
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# [Optional] Update UID/GID if needed
RUN if [ "$USER_GID" != "1000" ] || [ "$USER_UID" != "1000" ]; then \
        groupmod --gid $USER_GID $USERNAME \
        && usermod --uid $USER_UID --gid $USER_GID $USERNAME \
        && chown -R $USER_UID:$USER_GID /home/$USERNAME; \
    fi

# NOTE: we need to set HOME, because elm uses $HOME/.elm to save packages.
#       If this doesn't persist between runs, you get CORRUPT BINARY errors 
#       or the Elm VS Code extension stops working.
ENV HOME /cache

# Install elm and additional programs
RUN export DEBIAN_FRONTEND=noninteractive \
    && sudo -u ${USERNAME} npm install --global \
    npm \
    elm@${ELM_VERSION} \
    elm-test@${ELM_TEST_VERSION} \
    elm-format@${ELM_FORMAT_VERSION} \
	elm-review@${ELM_REVIEW_VERSION} \
    create-elm-app@${CREATE_ELM_APP_VERSION}

# Install mob.sh
RUN curl -sL install.mob.sh | sh
