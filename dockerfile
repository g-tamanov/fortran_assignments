FROM centos:latest
MAINTAINER Jessica Kelly <jkelly@urban.org>
RUN yum update -y
# add gfortran, debugging tools and make
RUN yum install -y gcc-gfortran gdb make

COPY ./code/make_and_run.sh /fortran/
WORKDIR /fortran/
RUN chmod +x make_and_run.sh

CMD ["/usr/sbin/init"]
