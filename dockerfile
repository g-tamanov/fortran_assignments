FROM centos:latest
MAINTAINER Jessica Kelly <jkelly@urban.org>
RUN yum update -y
# add gfortran, debugging tools and make
RUN yum install -y gcc-gfortran gdb make
#fortran 9
#RUN dnf -y install gcc-toolset-9-gcc gcc-toolset-9-gcc-c++
#RUN export PATH=/opt/rh/gcc-toolset-9/root/usr/bin:$PATH

COPY ./code/make_and_run.sh /fortran/
WORKDIR /fortran/
RUN chmod +x make_and_run.sh

CMD ["/usr/sbin/init"]
