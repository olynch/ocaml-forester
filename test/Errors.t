  $ cd forest
  $ forester build error-no-theme.toml
   ￫ error[Reporter.Message.Configuration_error]
   ꭍ ￮ when copying theme directory
   ￮ No such file or directory: nonexistent
  
  [1]
  $ forester build error-no-assets.toml
   ￫ error[Reporter.Message.Configuration_error]
   ꭍ ￮ when planting assets
   ￮ No such file or directory: nonexistent
  
  [1]
  $ forester build error-no-trees.toml
   ￫ error[Reporter.Message.Configuration_error]
   ꭍ ￮ when loading trees from disk
   ￮ No such file or directory: nonexistent
  
  [1]
