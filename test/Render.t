  $ cd forest
  $ forester render --addr=index --format=xml
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
