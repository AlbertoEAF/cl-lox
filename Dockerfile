FROM albertoeafworks/roswell

RUN mkdir cl-lox
WORKDIR cl-lox

# Setup the libraries separately for faster image rebuilds.
COPY cl-lox.asd .
RUN echo "Installing cl-lox libraries..." \
 && ros run --eval '(ql:quickload (getf (uiop:read-file-form "cl-lox.asd") :depends-on)) (exit)'

# Compile for fast execution
COPY . .
RUN echo "Compiling cl-lox binary..." \
 && cd roswell && ros build cl-lox.ros && mv cl-lox "$ROSWELL_BIN/cl-lox"


