#!/bin/sh
set -e -u
pl=$1

ocamlbuild p_${pl}.native
mkdir -p stage/$pl/src

cp -p p_${pl}.native stage/$pl/run

cat > stage/$pl/install <<EOT
#!/bin/sh
exit 0
EOT
chmod +x stage/$pl/install

git ls-tree -r --name-only HEAD | cpio -pdlmv stage/$pl/src

( cd stage/$pl; tar czvf ../$pl.tar.gz . )
