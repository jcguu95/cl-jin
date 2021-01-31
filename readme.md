# Usage

1. Copy this directory to root's quicklisp local project.
2. Make sure `/root/.sbclrc` loads `quicklisp` properly.
3. Run the command.

   `sbcl --script ./bin/run.script.lisp`

# Remark

Notice that the functions `#'url-lock` and `#'url-unlock` do
REQUIRE access to `/etc/hosts`.

# Bug

Remember that `#'lock-url` has a weird bug.
