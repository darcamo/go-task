
# go-task

Provides an interface to use `go-task` from Emacs. It allows you to run `go-task` commands and manage your tasks
directly from Emacs.



## Configuration

Customize `go-task-command` if the `go-task` executable is not on your `PATH` or you need to point Emacs to a wrapper script.

## Commands

- `M-x go-task-init` — runs `go-task --init` in the current directory and reports an error if the Taskfile already exists.
- `M-x go-task-run-task` — lists available tasks with `completing-read` and executes the selection. Call with `C-u` to run the default task without prompting.
- `M-x go-task-list-tasks` — shows the raw `go-task --list` output in a read-only buffer for quick inspection.
