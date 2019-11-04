![modules](./misc/generated/sloc-bowtie.svg)

# Design

Module layout:

![modules](./misc/generated/modules.svg)

Legend: `---->` means "imports".

```sh
stack install bowtie # all these command from the root of the repo

bowtie ./example-app/lunar-lander.bowtie
-- this evaluates lunar-lander and prints the result to the screen.
-- to get this into a webpage and actually play it see ../bowtie-js
```
