# levantemodels: LEVANTE Data Processing and Modeling

Internal tooling for processing LEVANTE data and fitting and serving its
psychometric (IRT) scoring models. Holds the trial recoding, scoring
pipeline, model registry, and ModelRecord machinery. For loading LEVANTE
data, see the levante package.

## Options

- `levantemodels.cache_dir`: directory in which to cache the metadata
  tables fetched from `levante_metadata_items` and
  `levante_metadata_scoring`
  ([`fetch_corpus_items()`](https://levante-framework.github.io/levantemodels/reference/fetch_corpus_items.md),
  [`fetch_scoring_table()`](https://levante-framework.github.io/levantemodels/reference/fetch_scoring_table.md),
  etc.), so that later sessions read them from disk instead of
  downloading them again. Only released dataset versions are cached.
  Defaults to `NULL` (no caching).

## See also

Useful links:

- <https://levante-framework.github.io/levantemodels/>

## Author

**Maintainer**: Mika Braginsky <mika.br@gmail.com>
([ORCID](https://orcid.org/0000-0001-9039-3220))

Authors:

- Michael Frank <mcfrank@stanford.edu>
  ([ORCID](https://orcid.org/0000-0002-7551-4378))

Other contributors:

- Theresa Cheng <twcheng@stanford.edu>
  ([ORCID](https://orcid.org/0000-0002-8557-565X)) \[contributor\]
