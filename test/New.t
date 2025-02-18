  $ cd forest
  $ ls trees
  asset.tree
  figure.tree
  hello.tree
  index.tree
  lorem.tree
  nested
  person.tree
  $ forester new --prefix=foo no-export.toml
   ￫ info[Reporter.Message.Log]
   ￮ Building ./build/resources/4f2455dfdf10f6ad466d28c223f6bc39.svg
  
  ./trees/foo-0001.tree
  $ ls trees
  asset.tree
  figure.tree
  foo-0001.tree
  hello.tree
  index.tree
  lorem.tree
  nested
  person.tree
  $ forester new --prefix=foo no-export.toml
  ./trees/foo-0002.tree
  $ mkdir dest
  $ forester new --prefix=foo --dest=dest no-export.toml
  ./dest/foo-0003.tree
  $ ls trees
  asset.tree
  figure.tree
  foo-0001.tree
  foo-0002.tree
  hello.tree
  index.tree
  lorem.tree
  nested
  person.tree
  $ forester new --prefix=foo --dest=nonexistent no-export.toml
  forester: option '--dest': no 'nonexistent' directory
  Usage: forester new [OPTION]… [FOREST]
  Try 'forester new --help' or 'forester --help' for more information.
  [124]
