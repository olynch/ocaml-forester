Run build:

  $ cd forest
  $ forester export export.toml
  $ forester build forest.toml
   ￫ info[Reporter.Message.Log]
   ￮ Building ./build/resources/4f2455dfdf10f6ad466d28c223f6bc39.svg
  

  $ cat output/forest.json
  {"hello":{"title":"Hello","taxon":null,"tags":[],"route":"hello.xml","metas":{}},"forest://foreign/index":{"title":"I am exported","taxon":null,"tags":[],"route":"foreign-foreign-index.xml","metas":{}},"index":{"title":"Hello","taxon":null,"tags":[],"route":"index.xml","metas":{}},"nested":{"title":"I am nested","taxon":null,"tags":[],"route":"nested.xml","metas":{}},"lorem":{"title":"Forest://lsp-test/lorem","taxon":null,"tags":[],"route":"lorem.xml","metas":{}},"asset":{"title":"Forest://lsp-test/asset","taxon":null,"tags":[],"route":"asset.xml","metas":{}},"hash/4f2455dfdf10f6ad466d28c223f6bc39":{"title":"Forest://lsp-test/hash/4f2455dfdf10f6ad466d28c223f6bc39","taxon":null,"tags":[],"route":"hash-4f2455dfdf10f6ad466d28c223f6bc39.xml","metas":{}},"person":{"title":"Author Testington","taxon":"Person","tags":[],"route":"person.xml","metas":{}},"figure":{"title":"Forest://lsp-test/figure","taxon":null,"tags":[],"route":"figure.xml","metas":{}},"foo-0001":{"title":"Forest://lsp-test/foo-0001","taxon":null,"tags":[],"route":"foo-0001.xml","metas":{}}}

  $ ls output
  asset.xml
  figure.xml
  foo-0001.xml
  foreign-foreign-index.xml
  forest.json
  hash-4f2455dfdf10f6ad466d28c223f6bc39.xml
  hash-bafkrmicdssbzi7prmhx4kqzm6cq5saqjawd5kxye7c4nep3a2ew7sx7aou.md
  hello.xml
  index.xml
  lorem.xml
  nested.xml
  person.xml
  $ cat output/index.xml
  <?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree xmlns:fr="http://www.jonmsterling.com/jms-005P.xml" root="true">
    <fr:frontmatter>
      <fr:authors />
      <fr:anchor>422</fr:anchor>
      <fr:addr type="user">index</fr:addr>
      <fr:route>index.xml</fr:route>
      <fr:title text="Hello">Hello</fr:title>
    </fr:frontmatter>
    <fr:mainmatter />
    <fr:backmatter>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>417</fr:anchor>
          <fr:title text="References">References</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>418</fr:anchor>
          <fr:title text="Context">Context</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>419</fr:anchor>
          <fr:title text="Backlinks">Backlinks</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>420</fr:anchor>
          <fr:title text="Related">Related</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>421</fr:anchor>
          <fr:title text="Contributions">Contributions</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
    </fr:backmatter>
  </fr:tree>
