#!/bin/sh

# Based on https://github.com/yamadapc/stack-gh-pages
set -o errexit
set -o xtrace

mkdocs() {
  package=$1
  stack_yaml=$2
  parent_dir=$(pwd)

  docsdir="$parent_dir/haddocks"

  cd "$package"

  stack --stack-yaml "$stack_yaml" \
       haddock \
       --haddock-hyperlink-source \
       --no-haddock-deps \
       --force-dirty \
       --haddock-arguments --odir=temp-docs

  me=$(whoami)
  sudo chown -R "$me"."$me" temp-docs
  cd "$parent_dir"
  rm -rf "$docsdir"
  mv "$package/temp-docs" "$docsdir"
}

mkindex() {
cat << END > index.html
<!DOCTYPE html>
<html>
  <head>
    <title>Shrubbery Documentation</title>
  </head>
  <body>
    <h1>Documentation for yet-to-be released Shrubbery packages</h1>
    <ul>
      <li><a href="haddocks">shrubbery</a></li>
    </ul>
  </body>
</html>
END
}

push_to_pages_branch() {
  git stash
  git branch -D gh-pages || echo "No existing pages branch to delete"
  git checkout --orphan gh-pages
  git add .
  git commit -m "Automated Haddock commit"
  git checkout main

  echo "Docs have been generated on the gh-pages branch."
  echo "You can push make them live with the following command:"
  echo ""
  echo "  git push -f -u origin gh-pages:gh-pages"
}

mkdocs . stack.yaml
mkindex
push_to_pages_branch
