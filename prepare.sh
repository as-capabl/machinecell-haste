#!/bin/bash

ISSUE_CUSTOM="distributive comonad semigroupoids"
ISSUE_ANN="profunctors"

#
# Initial check
#
if [ -e packages ]
then
    read -p "Directory \'packages\' will be initialized. Say y if you continue." -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        exit 1
    fi
    rm -rf packages
fi
mkdir -p packages

if [ ! -e cabal.sandbox.config ]
then
    haste-cabal sandbox init
fi

#
# Prepare src (workaround https://github.com/valderman/haste-compiler/issues/124)
#
for pkg in $ISSUE_CUSTOM
do
    haste-cabal unpack $pkg -d packages/
    sed -i 's/[ \t]*build-type:[ \t].*Custom/build-type: Simple/' packages/$pkg-*/$pkg.cabal
    haste-cabal sandbox add-source packages/$pkg-*
done

#
# Prepare src (workaround https://github.com/valderman/haste-compiler/issues/113)
#
for pkg in $ISSUE_ANN
do
    haste-cabal unpack $pkg -d packages/
    find packages/$pkg-* -name '*.hs'|xargs -n 1 sed -i 's/{-#\([ \t]*ANN.*\)#-}/{-\1-}/'
    haste-cabal sandbox add-source packages/$pkg-*
done

#
# Prepare src machinecell(branch)
#
git clone http://github.com/as-capabl/machinecell.git packages/machinecell-haste
git -C packages/machinecell-haste checkout haste
haste-cabal sandbox add-source packages/machinecell-haste

#
# Install packages
#
haste-cabal install -f-integer-gmp hashable

for pkg in $ISSUE_CUSTOM $ISSUE_ANN
do
    haste-cabal install $pkg
done

haste-cabal install -f-arrow-tr machinecell

