FROM albertoeafworks/roswell

RUN mkdir -p cl-lox/src
WORKDIR cl-lox

# Setup the libraries separately for faster image rebuilds.
COPY src/cl-lox.asd src/
RUN echo "Installing cl-lox libraries..." \
 && ros run --eval '(ql:quickload (getf (uiop:read-file-form "src/cl-lox.asd") :depends-on)) (exit)'

# Compile for fast execution
COPY . .
RUN echo "Compiling cl-lox binary..." \
 && cd roswell && ros build cl-lox.ros && mv cl-lox "$ROSWELL_BIN/cl-lox"


