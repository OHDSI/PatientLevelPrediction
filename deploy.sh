#!/bin/bash
set -o errexit -o nounset
addToDrat(){
  PKG_REPO=$PWD

  ## Build package tar ball
  export PKG_TARBALL=$(ls *.tar.gz)

  cd ..; mkdir drat; cd drat

  ## Set up Repo parameters
  git init
  git config user.name "Martijn Schuemie"
  git config user.email "schuemie@ohdsi.org"
  git config --global push.default simple

  ## Get drat repo
  git remote add upstream "https://$GH_TOKEN@github.com/OHDSI/drat.git"
  git fetch upstream 2>err.txt
  git checkout gh-pages
  
  ## Link to local R packages  
  echo 'R_LIBS=~/Rlib' > .Renviron
 
  Rscript -e "drat::insertPackage('$PKG_REPO/$PKG_TARBALL', \
    repodir = '.', \
    commit='GitHub Actions release: $PKG_TARBALL run $GITHUB_RUN_ID')"
  git push

}
addToDrat
