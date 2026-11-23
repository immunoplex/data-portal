# Use of generative AI in this repository

*Last reviewed: 2026-11-23.*

This file records changes to this repository that were made with the help of a
generative AI assistant (Anthropic's Claude, used through the Claude Code
command-line tool). The I-SPI project's full statement is
[`AI_USE.md` in immunoplex/i-spi](https://github.com/immunoplex/i-spi/blob/main/AI_USE.md).

## I-SPI migration source (2026-11)

* **What the assistant did.** It traced every query the I-SPI migration sends to
  the I-SPI database. It moved those queries to the `ispi_export` schema (the
  current fits in `standard_curve`, `model_qc` and `sample_qc`, and access-key
  checks through `check_access_key()`). It limited the source reads to the
  validated project, passed values as query parameters, and added the fit-method
  selector. It checked each new query against the production I-SPI database
  with the portal's read-only role.
* **What people did.** They decided to replace the retired `best_*` tables with
  the current fits, to offer both fit methods with frequentist as the default,
  and to give the portal a read-only database role. They reviewed the change
  and merged it through a pull request.

The maintainers are responsible for everything merged. The AI is not an author.
