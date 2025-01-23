  $ mkdir new-forest
  $ cd new-forest
  $ forester init
  Initialized empty Git repository in $TESTCASE_ROOT/new-forest/.git/
  Cloning into '$TESTCASE_ROOT/new-forest/theme'...
  Note: switching to '4.3.0'.
  
  You are in 'detached HEAD' state. You can look around, make experimental
  changes and commit them, and you can discard any commits you make in this
  state without impacting any branches by switching back to a branch.
  
  If you want to create a new branch to retain commits you create, you may
  do so (now or later) by using -c with the switch command. Example:
  
    git switch -c <new-branch-name>
  
  Or undo this operation with:
  
    git switch -
  
  Turn off this advice by setting config variable advice.detachedHead to false
  
  HEAD is now at 7dc5c6d don't render backmatter on designated root (TODO: replace by customisation)
   ￫ info[Reporter.Message.Log]
   ￮ Initialized forest, try editing `trees/index.tree` and running `forester build`. Afterwards, you can open `output/index.xml` in your browser to view your forest.
  
  $ forester build -v
  forester: [INFO] Parse trees...
  
  forester: [INFO] Expand, evaluate and analyse forest...
  
  $ ls
  assets
  forest.toml
  output
  theme
  trees

