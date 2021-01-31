# Usage

Copy this directory to root's quicklisp local project.
As root, run `sbcl` and evaluate `(ql:quickload :curfew)`.
This will invoke the function `#'main`.

Notice that the functions `#'url-lock` and `#'url-unlock` do
REQUIRE access to `/etc/hosts`.

# Bug

Remember that `#'lock-url` has a weird bug.
