Run build:

  $ cd cram-forest
  $ forester build forest.toml

  $ cat output/forest.json
  {"index":{"title":"Hello","taxon":null,"tags":[],"route":"index.xml","metas":{}}}

  $ ls output
  forest.json
  index.xml
  $ cat output/index.xml
  <?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree xmlns:fr="http://www.jonmsterling.com/jms-005P.xml" root="true">
    <fr:frontmatter>
      <fr:authors />
      <fr:anchor>381</fr:anchor>
      <fr:addr type="user">index</fr:addr>
      <fr:route>index.xml</fr:route>
      <fr:title text="Hello">Hello</fr:title>
    </fr:frontmatter>
    <fr:mainmatter />
    <fr:backmatter>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>376</fr:anchor>
          <fr:title text="References">References</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>377</fr:anchor>
          <fr:title text="Context">Context</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>378</fr:anchor>
          <fr:title text="Backlinks">Backlinks</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>379</fr:anchor>
          <fr:title text="Related">Related</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
      <fr:tree show-metadata="false" hidden-when-empty="true">
        <fr:frontmatter>
          <fr:anchor>380</fr:anchor>
          <fr:title text="Contributions">Contributions</fr:title>
        </fr:frontmatter>
        <fr:mainmatter />
      </fr:tree>
    </fr:backmatter>
  </fr:tree>
