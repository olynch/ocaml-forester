Run build:

  $ cd forest
  $ forester build
   ￮ Parse trees...
  
   ￮ Expand, evaluate, and analyse forest...
  
   ￮ Render forest...
  

  $ cat output/*
  {"hello":{"title":"Hello","taxon":null,"tags":[],"route":"hello.xml","metas":{}},"nested":{"title":"I am nested","taxon":null,"tags":[],"route":"nested.xml","metas":{}},"lorem":{"title":"Forest://lsp-test/lorem","taxon":null,"tags":[],"route":"lorem.xml","metas":{}},"person":{"title":"Author Testington","taxon":"Person","tags":[],"route":"person.xml","metas":{}}}<?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree
  xmlns:fr="http://www.jonmsterling.com/jms-005P.xml"
  root="false"><fr:frontmatter><fr:authors><fr:author><fr:link
  href="person.xml"
  title="Author Testington"
  addr="person"
  type="local">Author Testington</fr:link></fr:author></fr:authors><fr:anchor>388</fr:anchor><fr:addr
  type="user">hello</fr:addr><fr:route>hello.xml</fr:route><fr:title
  text="Hello">Hello</fr:title></fr:frontmatter><fr:mainmatter><fr:tree
  show-metadata="false"><fr:frontmatter><fr:authors /><fr:anchor>387</fr:anchor><fr:addr
  type="user">lorem</fr:addr><fr:route>lorem.xml</fr:route><fr:title
  text="" /></fr:frontmatter><fr:mainmatter><fr:p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla imperdiet tempus mauris vel suscipit. Vestibulum tincidunt turpis et risus vulputate volutpat. Interdum et malesuada fames ac ante ipsum primis in faucibus. Quisque sit amet nisl et diam eleifend facilisis. Aliquam porta, turpis in volutpat congue, mi ligula dictum nunc, ac mollis erat ex eget nunc. Vestibulum at posuere quam. Proin facilisis porta erat, et feugiat libero egestas at. Morbi rhoncus fringilla dolor, sit amet egestas lectus dictum vel. Curabitur arcu lectus, feugiat a nibh nec, scelerisque porta augue. Mauris interdum magna odio, sed efficitur turpis fermentum at. Nunc imperdiet metus sit amet nulla ornare condimentum. Cras id auctor sem.</fr:p></fr:mainmatter></fr:tree></fr:mainmatter><fr:backmatter><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>382</fr:anchor><fr:title
  text="References">References</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>383</fr:anchor><fr:title
  text="Context">Context</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>384</fr:anchor><fr:title
  text="Backlinks">Backlinks</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>385</fr:anchor><fr:title
  text="Related">Related</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>386</fr:anchor><fr:title
  text="Contributions">Contributions</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree></fr:backmatter></fr:tree><?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree
  xmlns:fr="http://www.jonmsterling.com/jms-005P.xml"
  root="false"><fr:frontmatter><fr:authors /><fr:anchor>401</fr:anchor><fr:addr
  type="user">lorem</fr:addr><fr:route>lorem.xml</fr:route><fr:title
  text="" /></fr:frontmatter><fr:mainmatter><fr:p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla imperdiet tempus mauris vel suscipit. Vestibulum tincidunt turpis et risus vulputate volutpat. Interdum et malesuada fames ac ante ipsum primis in faucibus. Quisque sit amet nisl et diam eleifend facilisis. Aliquam porta, turpis in volutpat congue, mi ligula dictum nunc, ac mollis erat ex eget nunc. Vestibulum at posuere quam. Proin facilisis porta erat, et feugiat libero egestas at. Morbi rhoncus fringilla dolor, sit amet egestas lectus dictum vel. Curabitur arcu lectus, feugiat a nibh nec, scelerisque porta augue. Mauris interdum magna odio, sed efficitur turpis fermentum at. Nunc imperdiet metus sit amet nulla ornare condimentum. Cras id auctor sem.</fr:p></fr:mainmatter><fr:backmatter><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>395</fr:anchor><fr:title
  text="References">References</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>397</fr:anchor><fr:title
  text="Context">Context</fr:title></fr:frontmatter><fr:mainmatter><fr:tree
  show-metadata="true"
  expanded="false"
  toc="false"
  numbered="false"><fr:frontmatter><fr:authors><fr:author><fr:link
  href="person.xml"
  title="Author Testington"
  addr="person"
  type="local">Author Testington</fr:link></fr:author></fr:authors><fr:anchor>396</fr:anchor><fr:addr
  type="user">hello</fr:addr><fr:route>hello.xml</fr:route><fr:title
  text="Hello">Hello</fr:title></fr:frontmatter><fr:mainmatter><fr:tree
  show-metadata="false"><fr:frontmatter><fr:authors /><fr:anchor>387</fr:anchor><fr:addr
  type="user">lorem</fr:addr><fr:route>lorem.xml</fr:route><fr:title
  text="" /></fr:frontmatter><fr:mainmatter><fr:p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla imperdiet tempus mauris vel suscipit. Vestibulum tincidunt turpis et risus vulputate volutpat. Interdum et malesuada fames ac ante ipsum primis in faucibus. Quisque sit amet nisl et diam eleifend facilisis. Aliquam porta, turpis in volutpat congue, mi ligula dictum nunc, ac mollis erat ex eget nunc. Vestibulum at posuere quam. Proin facilisis porta erat, et feugiat libero egestas at. Morbi rhoncus fringilla dolor, sit amet egestas lectus dictum vel. Curabitur arcu lectus, feugiat a nibh nec, scelerisque porta augue. Mauris interdum magna odio, sed efficitur turpis fermentum at. Nunc imperdiet metus sit amet nulla ornare condimentum. Cras id auctor sem.</fr:p></fr:mainmatter></fr:tree></fr:mainmatter></fr:tree></fr:mainmatter></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>398</fr:anchor><fr:title
  text="Backlinks">Backlinks</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>399</fr:anchor><fr:title
  text="Related">Related</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>400</fr:anchor><fr:title
  text="Contributions">Contributions</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree></fr:backmatter></fr:tree><?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree
  xmlns:fr="http://www.jonmsterling.com/jms-005P.xml"
  root="false"><fr:frontmatter><fr:authors /><fr:anchor>394</fr:anchor><fr:addr
  type="user">nested</fr:addr><fr:route>nested.xml</fr:route><fr:title
  text="I am nested">I am nested</fr:title></fr:frontmatter><fr:mainmatter /><fr:backmatter><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>389</fr:anchor><fr:title
  text="References">References</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>390</fr:anchor><fr:title
  text="Context">Context</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>391</fr:anchor><fr:title
  text="Backlinks">Backlinks</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>392</fr:anchor><fr:title
  text="Related">Related</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>393</fr:anchor><fr:title
  text="Contributions">Contributions</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree></fr:backmatter></fr:tree><?xml version="1.0" encoding="UTF-8"?>
  <?xml-stylesheet type="text/xsl" href="default.xsl"?>
  <fr:tree
  xmlns:fr="http://www.jonmsterling.com/jms-005P.xml"
  root="false"><fr:frontmatter><fr:authors /><fr:anchor>408</fr:anchor><fr:addr
  type="user">person</fr:addr><fr:route>person.xml</fr:route><fr:title
  text="Author Testington">Author Testington</fr:title><fr:taxon>Person</fr:taxon></fr:frontmatter><fr:mainmatter /><fr:backmatter><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>402</fr:anchor><fr:title
  text="References">References</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>403</fr:anchor><fr:title
  text="Context">Context</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>404</fr:anchor><fr:title
  text="Backlinks">Backlinks</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>405</fr:anchor><fr:title
  text="Related">Related</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree
  show-metadata="false"
  hidden-when-empty="true"><fr:frontmatter><fr:anchor>407</fr:anchor><fr:title
  text="Contributions">Contributions</fr:title></fr:frontmatter><fr:mainmatter><fr:tree
  show-metadata="true"
  expanded="false"
  toc="false"
  numbered="false"><fr:frontmatter><fr:authors><fr:author><fr:link
  href="person.xml"
  title="Author Testington"
  addr="person"
  type="local">Author Testington</fr:link></fr:author></fr:authors><fr:anchor>406</fr:anchor><fr:addr
  type="user">hello</fr:addr><fr:route>hello.xml</fr:route><fr:title
  text="Hello">Hello</fr:title></fr:frontmatter><fr:mainmatter><fr:tree
  show-metadata="false"><fr:frontmatter><fr:authors /><fr:anchor>387</fr:anchor><fr:addr
  type="user">lorem</fr:addr><fr:route>lorem.xml</fr:route><fr:title
  text="" /></fr:frontmatter><fr:mainmatter><fr:p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla imperdiet tempus mauris vel suscipit. Vestibulum tincidunt turpis et risus vulputate volutpat. Interdum et malesuada fames ac ante ipsum primis in faucibus. Quisque sit amet nisl et diam eleifend facilisis. Aliquam porta, turpis in volutpat congue, mi ligula dictum nunc, ac mollis erat ex eget nunc. Vestibulum at posuere quam. Proin facilisis porta erat, et feugiat libero egestas at. Morbi rhoncus fringilla dolor, sit amet egestas lectus dictum vel. Curabitur arcu lectus, feugiat a nibh nec, scelerisque porta augue. Mauris interdum magna odio, sed efficitur turpis fermentum at. Nunc imperdiet metus sit amet nulla ornare condimentum. Cras id auctor sem.</fr:p></fr:mainmatter></fr:tree></fr:mainmatter></fr:tree></fr:mainmatter></fr:tree></fr:backmatter></fr:tree>
