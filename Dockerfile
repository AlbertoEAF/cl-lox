FROM albertoeafworks/roswell

# Setup the libraries separately for faster image rebuilds.
RUN mkdir cl-lox
COPY cl-lox.asd cl-lox
RUN echo "Installing cl-lox libraries..." \
 && ros run --eval '(ql:quickload (getf (uiop:read-file-form "cl-lox/cl-lox.asd") :depends-on)) (exit)'

# Compile for fast execution
COPY . cl-lox
RUN echo "Compiling cl-lox binary..." \
 && cd cl-lox/roswell && ros build cl-lox.ros && mv cl-lox "$ROSWELL_BIN/cl-lox"


