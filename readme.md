# Usage

1. Copy this directory to root's quicklisp local-projects directory.
2. Make sure `/root/.sbclrc` loads `quicklisp` properly.
3. Run the command `./bin/run.sh`.

# Remark

Notice that the functions `#'url-lock` and `#'url-unlock` do
REQUIRE access to `/etc/hosts`.

# Bug

Remember that `#'lock-url` has a weird bug.

# TODO

+ Add systematic 10-minute breaks.
+ Add systematic 30-minute locks.
