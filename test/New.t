  $ cd cram-forest
  $ ls trees
  index.tree
  $ forester new --prefix=foo
  ./trees/foo-0001.tree
  $ echo $NEW_TREE
  
  $ ls trees
  foo-0001.tree
  index.tree
  $ forester new --prefix=foo
  ./trees/foo-0002.tree
  $ ls trees
  foo-0001.tree
  foo-0002.tree
  index.tree
