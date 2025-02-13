  $ cd forest

Export the forest defined by `export.toml`

  $ forester export -v export.toml
  forester: [INFO] Parse trees...
  
  forester: [INFO] Expand, evaluate and analyse forest...
  
  forester: [INFO] Exporting forest...
  
Verify that the foreign blob can be implanted.

  $ forester build -v forest.toml
  forester: [INFO] Implant foreign forest from `$TESTCASE_ROOT/forest/export/foreign.json'...
  
  forester: [INFO] Parse trees...
  
  forester: [INFO] Expand, evaluate and analyse forest...
  
   ￫ info[Reporter.Message.Log]
   ￮ Building ./build/resources/4f2455dfdf10f6ad466d28c223f6bc39.svg
  
