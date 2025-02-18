Run build:

  $ cd forest
  $ forester export export.toml
  $ forester build forest.toml
   ￫ info[Reporter.Message.Log]
   ￮ Building ./build/resources/4f2455dfdf10f6ad466d28c223f6bc39.svg
  


  $ cat output/forest.json
  {"hello":{"title":"Hello","taxon":null,"tags":[],"route":"hello.xml","metas":{}},"forest://foreign/index":{"title":"I am exported","taxon":null,"tags":[],"route":"foreign-foreign-index.xml","metas":{}},"index/0":{"title":"Hello › I am an anonymous subtree","taxon":null,"tags":[],"route":"index-0.xml","metas":{}},"index":{"title":"Hello","taxon":null,"tags":[],"route":"index.xml","metas":{}},"sub":{"title":"Hello › I am a subtree","taxon":null,"tags":[],"route":"sub.xml","metas":{}},"nested":{"title":"I am nested","taxon":null,"tags":[],"route":"nested.xml","metas":{}},"lorem":{"title":"Forest://lsp-test/lorem","taxon":null,"tags":[],"route":"lorem.xml","metas":{}},"asset":{"title":"Forest://lsp-test/asset","taxon":null,"tags":[],"route":"asset.xml","metas":{}},"hash/4f2455dfdf10f6ad466d28c223f6bc39":{"title":"Forest://lsp-test/hash/4f2455dfdf10f6ad466d28c223f6bc39","taxon":null,"tags":[],"route":"hash-4f2455dfdf10f6ad466d28c223f6bc39.xml","metas":{}},"person":{"title":"Author Testington","taxon":"Person","tags":[],"route":"person.xml","metas":{}},"figure":{"title":"Forest://lsp-test/figure","taxon":null,"tags":[],"route":"figure.xml","metas":{}}}

  $ ls output
  asset.xml
  figure.xml
  foreign-foreign-index.xml
  forest.json
  hash-4f2455dfdf10f6ad466d28c223f6bc39.xml
  hash-bafkrmicdssbzi7prmhx4kqzm6cq5saqjawd5kxye7c4nep3a2ew7sx7aou.md
  hello.xml
  index-0.xml
  index.xml
  lorem.xml
  nested.xml
  person.xml
  sub.xml
  $ cat output/index.xml
  <?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree xmlns:fr="http://www.jonmsterling.com/jms-005P.xml" root="true">
    <fr:frontmatter>
      <fr:authors />
      <fr:addr>index</fr:addr>
      <fr:route>index.xml</fr:route>
      <fr:title text="Hello">Hello</fr:title>
    </fr:frontmatter>
    <fr:mainmatter>
      <fr:tree show-metadata="false">
        <fr:frontmatter>
          <fr:authors />
          <fr:addr>sub</fr:addr>
          <fr:route>sub.xml</fr:route>
          <fr:title text="I am a subtree">I am a subtree</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false">
        <fr:frontmatter>
          <fr:authors />
          <fr:addr>index/0</fr:addr>
          <fr:route>index-0.xml</fr:route>
          <fr:title text="I am an anonymous subtree">I am an anonymous subtree</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
    </fr:mainmatter>
    <fr:backmatter>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:title text="References">References</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:title text="Context">Context</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:title text="Backlinks">Backlinks</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:title text="Related">Related</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:title text="Contributions">Contributions</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
    </fr:backmatter>
  </fr:tree>
<<<<<<< HEAD
=======
>>>>>>> 81943b1 (Delete Iri_resolver)
<<<<<<< HEAD
=======
>>>>>>> 81943b1 (Delete Iri_resolver)
<<<<<<< HEAD
=======
>>>>>>> 81943b1 (Delete Iri_resolver)
<<<<<<< HEAD
=======
>>>>>>> 81943b1 (Delete Iri_resolver)
<<<<<<< HEAD
=======
>>>>>>> 81943b1 (Delete Iri_resolver)
<<<<<<< HEAD
=======
>>>>>>> 81943b1 (Delete Iri_resolver)
