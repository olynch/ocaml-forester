  $ transclude
  <?xml version="1.0" encoding="UTF-8"?>
  
  <fr:tree xmlns:fr="http://www.jonmsterling.com/jms-005P.xml" root="false">
    <fr:frontmatter>
      <fr:authors />
      <fr:addr>transcludee</fr:addr>
      <fr:route>transcludee.xml</fr:route>
      <fr:title text="" />
    </fr:frontmatter>
    <fr:mainmatter>
      <fr:tree show-metadata="false">
        <fr:frontmatter>
          <fr:authors />
          <fr:addr>transcludee</fr:addr>
          <fr:route>transcludee.xml</fr:route>
          <fr:title text="I am being transcluded">I am being transcluded</fr:title>
        </fr:frontmatter>
        <fr:mainmatter>
          <fr:info>Transclusion loop detected, rendering stopped.</fr:info>
        </fr:mainmatter>
      </fr:tree>
    </fr:mainmatter>
    <fr:backmatter />
  </fr:tree>
  <?xml version="1.0" encoding="UTF-8"?>
  
  <fr:tree xmlns:fr="http://www.jonmsterling.com/jms-005P.xml" root="false">
    <fr:frontmatter>
      <fr:authors />
      <fr:addr>transcludee</fr:addr>
      <fr:route>transcludee.xml</fr:route>
      <fr:title text="" />
    </fr:frontmatter>
    <fr:mainmatter>I am being transcluded</fr:mainmatter>
    <fr:backmatter />
  </fr:tree>
  <?xml version="1.0" encoding="UTF-8"?>
  
  <fr:tree xmlns:fr="http://www.jonmsterling.com/jms-005P.xml" root="false">
    <fr:frontmatter>
      <fr:authors />
      <fr:addr>transcludee</fr:addr>
      <fr:route>transcludee.xml</fr:route>
      <fr:title text="" />
    </fr:frontmatter>
    <fr:mainmatter>
      <fr:tree show-metadata="true">
        <fr:frontmatter>
          <fr:authors />
          <fr:addr>transcludee</fr:addr>
          <fr:route>transcludee.xml</fr:route>
          <fr:title text="I am being transcluded">I am being transcluded</fr:title>
        </fr:frontmatter>
        <fr:mainmatter>
          <fr:info>Transclusion loop detected, rendering stopped.</fr:info>
        </fr:mainmatter>
      </fr:tree>
    </fr:mainmatter>
    <fr:backmatter />
  </fr:tree>
