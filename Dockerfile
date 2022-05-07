# syntax=docker/dockerfile:1
ARG FROM
FROM ${FROM}

LABEL maintainer="mejla@software.se"
LABEL vendor="Y Software AB"
LABEL url="https://github.com/for-get/jesse"
LABEL vcs-url="https://github.com/for-get/jesse"
ARG LABEL_VCS_REF="0"
LABEL vcs-ref=${LABEL_VCS_REF}
ARG LABEL_BUILD_DATE="1970-01-01T00:00:00Z"
LABEL build-date=${LABEL_BUILD_DATE}



COPY . /jesse
RUN cd /jesse && ./Dockerfile.build.sh
RUN cd /jesse && make

CMD /jesse/bin/jesse
