<!-- Hey, emacs, please consider this to be -*- xml -*-

    This is an alternative to html.xsl for single-page output.
  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:macro="http://crategus.com/macro"
                version="1.0">

  <xsl:import  href="html-common.tmp"/>
  <xsl:include href="base-uri.xsl"/>
  <xsl:output  method="xml" indent="yes"/>

  <xsl:key name="about-symbol"      match="about-symbol"      use="@id"/>
  <xsl:key name="about-type"        match="about-type"        use="@id"/>
  <xsl:key name="about-variable"    match="about-variable"    use="@id"/>

  <xsl:key name="about-function"    match="about-function"    use="@id"/>
  <xsl:key name="about-generic"     match="about-generic"     use="@id"/>
  <xsl:key name="about-operator"    match="about-operator"    use="@id"/>
  <xsl:key name="about-macro"       match="about-macro"       use="@id"/>
  <xsl:key name="about-class"       match="about-class"       use="@id"/>
  <xsl:key name="about-struct"      match="about-struct"      use="@id"/>
  <xsl:key name="about-condition"   match="about-condition"   use="@id"/>

  <xsl:key name="id"
           match="symbol-definition |
                  type-definition |
                  variable-definition |
                  function-definition |
                  generic-definition |
                  operator-definition |
                  macro-definition |
                  gobject-class-definition |
                  standard-class-definition |
                  system-class-definition |
                  structure-class-definition |
                  built-in-class-definition |
                  condition-class-definition"
           use="@id"/>

<!--
  <xsl:key name="symbol-by-name"
           match="symbol-definition"
           use="@name"/>
-->

  <xsl:key name="symbol-by-name"
           match="symbol-definition"
           use="@id"/>

  <xsl:key name="type-by-name"
           match="type-definition"
           use="@id"/>
  <xsl:key name="variable-by-name"
           match="variable-definition"
           use="@id"/>

  <xsl:key name="function-by-name"
           match="function-definition"
           use="@id"/>

  <xsl:key name="generic-by-name"
           match="generic-definition"
           use="@id"/>
  <xsl:key name="operator-by-name"
           match="operator-definition"
           use="@id"/>
  <xsl:key name="macro-by-name"
           match="macro-definition"
           use="@id"/>

  <xsl:key name="standard-class-by-name"
           match="gobject-class-definition|type-definition"
           use="@id"/>
  <xsl:key name="standard-class-by-name"
           match="standard-class-definition|type-definition"
           use="@id"/>
  <xsl:key name="system-class-by-name"
           match="system-class-definition"
           use="@id"/>
  <xsl:key name="structure-class-by-name"
           match="structure-class-definition"
           use="@id"/>
  <xsl:key name="built-in-class-by-name"
           match="built-in-class-definition"
           use="@id"/>
  <xsl:key name="condition-class-by-name"
           match="condition-class-definition"
           use="@id"/>

  <xsl:template match="/">
    <pages>
      <xsl:call-template name="copy-base-uri"/>
      <macro:copy-attribute name="logo" path="documentation"/>
      <macro:copy-attribute name="css" path="documentation"/>
      <macro:copy-attribute name="heading" path="documentation"/>
      <macro:copy-attribute name="ico" path="documentation"/>
      <xsl:apply-templates select="documentation"/>
    </pages>
  </xsl:template>

<!-- Main template for the documentation -->
  <xsl:template match="documentation">
    <main-page title="{@index-title}"
               author="{/documentation/@author}"
               author-url="{/documentation/@author-url}"
               date="{/documentation/@date}"
               keywords="Lisp, Documentation, Package, {@name}"
               single-page="yes">
      <div id="sp-about-packages">
        <xsl:for-each select="package">
          <p>
            <i>About <xsl:value-of select="@name"/>:</i>
            <xsl:apply-templates select="documentation-string"/>
          </p>
        </xsl:for-each>
      </div>
      <xsl:if test="package/sections">
        <h3>Contents</h3>
        <div class="indent">
          <ul>
            <xsl:for-each select="package">
            <li>
              Package <xsl:value-of select="@name"/>
              <ul>
                <xsl:for-each select="sections/section">
                  <li>
                    <a href="#{generate-id()}">
                      <xsl:value-of select="@section"/>
                    </a>
                  </li>
                  </xsl:for-each>
                </ul>
              </li>
            </xsl:for-each>
          </ul>
        </div>
      </xsl:if>
      <xsl:apply-templates select="package"/>
      <h3 id="index">Index of Exported Symbols</h3>
      <simple-table>
        <xsl:apply-templates select="package/external-symbols/*"
                             mode="symbol-index">
          <xsl:sort select="@sortid" data-type="text" order="ascending"/>
          <xsl:with-param name="packagep" select="'pages/'"/>
        </xsl:apply-templates>
      </simple-table>
    </main-page>
  </xsl:template>

<!-- Template for each package -->
  <xsl:template match="package">
    <!-- Repeat package documentation -->
    <br/>
    <h2>Package <xsl:value-of select="@name"/></h2>
    <div class="noindent">
      <xsl:apply-templates select="documentation-string"/>
    </div>
    <!-- Documention for each section of the package -->
    <xsl:apply-templates select="sections/section"/>
    <!-- Handle undocumented function definition -->
    <xsl:variable name="unreferenced"
                  select="external-symbols/function-definition[
                          count(key('about-function',@id))=0]"/>
    <xsl:if test="$unreferenced">
      <h3>Other Functions in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented operator definition -->
    <xsl:variable name="unreferenced5"
                  select="external-symbols/operator-definition[
                          count(key('about-operator',@id))=0]"/>
    <xsl:if test="$unreferenced5">
      <h3>Other Special Operators in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced5">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented symbol definition -->
    <xsl:variable name="unreferenced6"
                  select="external-symbols/symbol-definition[
                          count(key('about-symbol',@id))=0]"/>
    <xsl:if test="$unreferenced6">
      <h3>Other Symbols in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced6">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented condition class definition -->
    <xsl:variable name="unreferenced7"
                  select="external-symbols/condition-class-definition[
                  count(key('about-condition',@id))=0]"/>
    <xsl:if test="$unreferenced7">
      <h3>Other Conditions in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced7">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented macro definition -->
    <xsl:variable name="unreferenced2"
                  select="external-symbols/macro-definition[
                          count(key('about-macro',@id))=0]"/>
    <xsl:if test="$unreferenced2">
      <h3>Other Macros in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced2">
    <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented gobject class definition -->
    <xsl:variable name="unreferenced1"
                  select="external-symbols/gobject-class-definition[
                          count(key('about-class',@id))=0]"/>
    <xsl:if test="$unreferenced1">
      <h3>Other Classes in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced1">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented standard class definition -->
    <xsl:variable name="unreferenced3"
                  select="external-symbols/standard-class-definition[
                          count(key('about-class',@id))=0]"/>
    <xsl:if test="$unreferenced3">
      <h3>Other Classes in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced3">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
    <!-- Handle undocumented gobject variable definition -->
    <xsl:variable name="unreferenced4"
                  select="external-symbols/variable-definition[
                          count(key('about-variable',@id))=0]"/>
    <xsl:if test="$unreferenced4">
      <h3>Other Variables in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced4">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
  </xsl:template>

<!-- Documentation of external symbols -->

  <!-- Header for external symbol on single page -->
  <xsl:template name="definition">
    <xsl:param name="label"/>
    <div id="{@id}" class="sp-lambda-list">
      <xsl:value-of select="@kindname"/>
      <xsl:text> </xsl:text>
      <xsl:value-of select="@package"/>
      <xsl:text>:</xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:if test="lambda-list">
        <xsl:text> (</xsl:text>
        <xsl:for-each select="lambda-list/elt">
          <xsl:if test="position() != 1">
            <xsl:text>&#160;</xsl:text>
          </xsl:if>
          <!-- Workaround for long lambda-list -->
          <xsl:if test="position() = 10">
            <xsl:text>&#xa;</xsl:text>
          </xsl:if>
          <xsl:value-of select="text()"/>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
      </xsl:if>
    </div>
  </xsl:template>

  <!-- Documentation for external symbol -->
  <xsl:template name="main-documentation-string">
    <xsl:if test="see-also/constructor">
      <div class="sph3">Returned by:</div>
      <div>
        <ul>
          <xsl:apply-templates select="see-also/constructor/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <div class="sph3">Slot Access Functions:</div>
      <div>
        <ul>
          <xsl:apply-templates select="see-also/slot/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="documentation-string">
        <div class="sph3">Details:</div>
        <xsl:apply-templates select="documentation-string"/>
      </xsl:when>
      <xsl:otherwise>
        <p style="color: red; font-weight: bold">
          No documentation string. Possibly unimplemented or incomplete.
        </p>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
    <xsl:apply-templates select="note"/>
    <xsl:if test="see-also/condition">
      <div class="sph3">Condition Types Signalled:</div>
      <div>
        <ul>
          <xsl:apply-templates select="see-also/condition/see"/>
        </ul>
      </div>
    </xsl:if>
    <xsl:if test="see-also/other|see-also/auto">
      <div class="sph3">See also:</div>
      <div class="indent">
        <simple-table>
          <xsl:for-each select="see-also/other/see|see-also/auto/see">
          <xsl:variable name="name" select="text()"/>
          <xsl:if test="not(preceding-sibling::see[text() = $name])">
            <xsl:choose>
              <xsl:when test="@id">
                <a href="#{@id}">
                  <span class="sym">
                    <xsl:apply-templates/>
                  </span>
                </a>
              </xsl:when>
              <xsl:otherwise>
                <span class="sym">
                  <xsl:apply-templates/>
                </span>
              </xsl:otherwise>
            </xsl:choose>
            <br/>
          </xsl:if>
        </xsl:for-each>
      </simple-table>
      </div>
    </xsl:if>
  </xsl:template>

  <!-- Documentation details for external symbol -->
  <xsl:template match="documentation-string">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <!-- Symbol definition -->
  <!-- Possible symbol aliases in GTK documentation:
       "Bitfield" "CEnum" "CStruct" "Callback" "Constant"
       "Enum" "GEnum" "GFlags" "Struct" "Type" "VTable" -->

  <xsl:template match="symbol-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Symbol'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="declaration"/>
        <xsl:apply-templates select="values"/>
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <!--
      Other templates
    -->

  <xsl:template name="main">
    <macro:maybe-columns test="see-also">
      <xsl:call-template name="main-left"/>
    </macro:maybe-columns>
  </xsl:template>

  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
        <a href="#{@id}">
          <tt>
            <xsl:choose>
              <xsl:when test="@status = 'INTERNAL'">
                <xsl:value-of select="@package"/>
                <xsl:text>::</xsl:text>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="@package"/>
                <xsl:text>:</xsl:text>
              </xsl:otherwise>
            </xsl:choose>
            <xsl:value-of select="@name"/>
          </tt>
        </a>
      </xsl:when>
      <xsl:when test="@status = 'INTERNAL'">
        <tt style="color: #777777">
          <xsl:value-of select="@package"/>
          <xsl:text>::</xsl:text>
          <xsl:value-of select="@name"/>
        </tt>
      </xsl:when>
      <xsl:otherwise>
        <tt style="color: #777777">
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </tt>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="lambda-list">
    <tt><xsl:value-of select="../@name"/></tt>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
    <xsl:text>&#160;</xsl:text>
      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <!-- The list of direct slots for a class definition -->
  <xsl:template name="slot-list">
    <div class="indent">
      <simple-table>
        <span class="table-entry">
          <span id="{@id}" class="code">
            <xsl:value-of select="@name"/>
          </span>
          <xsl:apply-templates select="documentation-string"/>
        </span>
      </simple-table>
    </div>
  </xsl:template>



  <xsl:template match="short">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="a">
    <a href="{@a}">
      <xsl:apply-templates/>
    </a>
  </xsl:template>

  <xsl:template match="url">
    <a href="{@url}">
      <xsl:apply-templates/>
    </a>
  </xsl:template>

  <xsl:template match="section">
    <a href="#{@id}">
        <xsl:apply-templates/>
    </a>
  </xsl:template>

  <xsl:template match="slot">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="#{@id}">
          <span class="code">
            <xsl:apply-templates/>
          </span>
        </a>
      </xsl:when>
      <xsl:otherwise>
        <span class="code">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="method">
    <span class="sym">
      <xsl:value-of select="@method"/>
    </span>
    <span class ="code">
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="see">
    <li>
      <a href="#{@id}">
    <tt>
      <xsl:apply-templates/>
    </tt>
      </a>
      <xsl:if test="@see">
    &#160;
    <i>
      <xsl:value-of select="@see"/>
    </i>
      </xsl:if>
    </li>
  </xsl:template>

  <!-- Dictionary Entries -->

  <xsl:template match="dictionary">
    <div class="noindent">
      <div class="sph3"><xsl:value-of select="@dictionary"/>:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="declaration">
    <div class="sph3">Declaration:</div>
    <div class="indent">
      <pre>
        <xsl:apply-templates/>
      </pre>
    </div>
  </xsl:template>

  <xsl:template match="values">
    <div class="sph3">Values:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

<!-- Improve this to include ID for references -->

  <xsl:template match="signal">
    <div id="{@id}" class="signal">
      <div class="sph5">
        The "<xsl:value-of select="@signal"/>" signal
      </div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="sig">
    <xsl:choose>
      <xsl:when test="@id">
          <span>"<a href="#{@id}">
            <span class="code">
              <xsl:apply-templates/>
            </span>
          </a>"</span>
      </xsl:when>
      <xsl:otherwise>
        <span class="code">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="property">
    <div id="{@id}" class="property">
      <div class="code">
        <xsl:value-of select="@property"/>
      </div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="prop">
    <xsl:choose>
      <xsl:when test="@id">
          <span><a href="#{@id}">
            <span class="code">
              <xsl:apply-templates/>
            </span>
          </a></span>
      </xsl:when>
      <xsl:otherwise>
        <span class="code">
          <xsl:apply-templates/>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="affected-by">
    <div class="noindent">
      <div class="sph3">Affected By:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="exceptional">
    <div class="noindent">
      <div class="sph3">Exceptional Situations:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

<!--
  <xsl:template match="notes">
    <div class="noindent">
      <div class="sph3">Notes:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>
-->

  <xsl:template match="methods">
    <div class="noindent">
      <div class="sph3">Methods:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-kind">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Kind:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-syntax">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Syntax:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-arguments">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Arguments:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="specifier-description">
    <div class="noindent">
      <div class="sph3">Compound Type Specifier Description:</div>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="supertypes">
    <div class="sph3">Supertypes:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="valid-context">
    <div class="sph3">Valid Context:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="binding-types">
    <div class="sph3">Binding Types Affected:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="syntax">
    <div class="sph3">Syntax:</div>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="syntax">
          <span class="syntax">
            <xsl:apply-templates/>
          </span>
          <br/>
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>

  <!-- End of Dictionary Entries -->

  <xsl:template match="implementation-note">
    <div class="sph3">Implementation notes:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="note">
    <div class="sph3">Notes:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="variable-type">
    <div class="sph3">Value Type:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="variable-value">
    <div class="sph3">Value:</div>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="heading">
    <div class="noindent">
      <div class="sph4">
        <xsl:apply-templates/>:
      </div>
    </div>
  </xsl:template>

  <xsl:template match="subheading">
    <div class="sph5">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="section">
    <h3 id="{generate-id()}">
<!--      <a name="{generate-id()}"/> -->
      <xsl:value-of select="@section"/>
    </h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="subsection">
    <div class="noindent">
      <h4 id="{@id}">
        <xsl:value-of select="@subsection"/>
      </h4>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

<!-- **********************************************************************  -->

  <xsl:template match="about-variable">
    <xsl:apply-templates select="key('variable-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-function">
    <xsl:apply-templates select="key('function-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-generic">
    <xsl:apply-templates select="key('generic-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-operator">
    <xsl:apply-templates select="key('operator-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-macro">
    <xsl:apply-templates select="key('macro-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-type">
    <xsl:apply-templates select="key('type-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-symbol">
    <xsl:apply-templates select="key('symbol-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-class">
    <xsl:apply-templates select="key('standard-class-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-struct">
    <xsl:apply-templates select="key('structure-class-by-name', current()/@id)"/>
  </xsl:template>

  <xsl:template match="about-condition">
    <xsl:apply-templates select="key('condition-class-by-name', current()/@id)"/>
  </xsl:template>

<!-- **********************************************************************  -->

  <xsl:template match="arguments">
    <div class="sph3">Arguments:</div>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="argument">
         <div class="indent-hanging">
          <xsl:choose>
            <xsl:when test="not(@argument='')">
              <span class="arg">
                <xsl:value-of select="@argument"/>
              </span>
              <xsl:text> -- </xsl:text>
              <xsl:apply-templates/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates/>
            </xsl:otherwise>
          </xsl:choose>
          <br/>
          </div>
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>



  <xsl:template match="type-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Type'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="declaration"/>
        <xsl:apply-templates select="values"/>
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="supertypes"/>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="variable-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Variable'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="variable-type"/>
        <xsl:apply-templates select="variable-value"/>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="function-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Function'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="generic-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Generic Function'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="operator-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Special Operator'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="macro-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Macro'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <xsl:apply-templates select="syntax"/>
        <xsl:apply-templates select="arguments"/>
        <xsl:if test="return">
          <div class="sph3">Returns:</div>
          <div class="indent">
            <xsl:apply-templates select="return/node()"/>
          </div>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>


  <xsl:template match="gobject-class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Class'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>
            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                <div class="indent">
                  None
                </div>
              </xsl:otherwise>
            </xsl:choose>
        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="standard-class-definition">
    <xsl:choose>
      <!-- Template for standard class with kindname GBoxed -->
      <xsl:when test="@kindname='GBoxed'">
        <xsl:call-template name="definition">
          <xsl:with-param name="label" select="'Class'"/>
        </xsl:call-template>
        <div class="sp-definition">
          <div class="sp-definition-body">
            <xsl:apply-templates select="declaration"/>
            <xsl:apply-templates select="values"/>
            <xsl:apply-templates select="syntax"/>
            <xsl:apply-templates select="arguments"/>
            <xsl:apply-templates select="valid-context"/>
            <xsl:apply-templates select="binding-types"/>
            <xsl:call-template name="main-documentation-string"/>
            <div id="version">
              <xsl:apply-templates select="version"/>
            </div>
          </div>
        </div>
      </xsl:when>
      <!-- This the template of a standard class with kindname Class -->
      <xsl:otherwise>
        <xsl:call-template name="definition">
          <xsl:with-param name="label" select="'Class'"/>
        </xsl:call-template>
        <div class="sp-definition">
          <div class="sp-definition-body">
            <div class="sph3">Superclasses:</div>
            <div class="indent">
              <xsl:for-each select="cpl/superclass">
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </div>
            <div class="sph3">Documented Subclasses:</div>
            <div class="indent">
              <xsl:choose>
                <xsl:when test="subclasses/subclass">
                  <xsl:for-each select="subclasses/subclass">
                    <xsl:sort select="@id" data-type="text" order="ascending"/>
                    <xsl:call-template name="class-list"/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  None
                </xsl:otherwise>
              </xsl:choose>
            </div>
            <xsl:if test="direct-slots">
              <div class="sph3">Direct Slots:</div>
              <xsl:choose>
                <xsl:when test="direct-slots/slot">
                  <xsl:for-each select="direct-slots/slot">
                    <xsl:sort select="@id" data-type="text" order="ascending"/>
                    <xsl:call-template name="slot-list"/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  <div class="indent">
                    None
                  </div>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:if>
            <xsl:call-template name="main-documentation-string"/>
            <div id="version">
              <xsl:apply-templates select="version"/>
            </div>
          </div>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="system-class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'System Class'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>

            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>

        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="structure-class-definition">
    <xsl:choose>
      <!-- Template for the structure class with kindname GBoxed -->
      <xsl:when test="@kindname='GBoxed'">
        <xsl:call-template name="definition">
          <xsl:with-param name="label" select="'Class'"/>
        </xsl:call-template>
        <div class="sp-definition">
          <div class="sp-definition-body">
            <xsl:apply-templates select="declaration"/>
            <xsl:apply-templates select="values"/>
            <xsl:apply-templates select="syntax"/>
            <xsl:apply-templates select="arguments"/>
            <xsl:apply-templates select="valid-context"/>
            <xsl:apply-templates select="binding-types"/>
            <xsl:call-template name="main-documentation-string"/>
            <div id="version">
              <xsl:apply-templates select="version"/>
            </div>
          </div>
        </div>
      </xsl:when>
      <!-- This the template of a standard class with kindname Class -->
      <xsl:otherwise>
        <xsl:call-template name="definition">
          <xsl:with-param name="label" select="'Struct'"/>
        </xsl:call-template>
        <div class="sp-definition">
          <div class="sp-definition-body">
            <div class="sph3">Superclasses:</div>
            <div class="indent">
              <xsl:for-each select="cpl/superclass">
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </div>
            <div class="sph3">Documented Subclasses:</div>
            <div class="indent">
              <xsl:choose>
                <xsl:when test="subclasses/subclass">
                  <xsl:for-each select="subclasses/subclass">
                    <xsl:sort select="@id" data-type="text" order="ascending"/>
                    <xsl:call-template name="class-list"/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  None
                </xsl:otherwise>
              </xsl:choose>
            </div>
            <xsl:if test="direct-slots">
              <div class="sph3">Direct Slots:</div>
              <xsl:choose>
                <xsl:when test="direct-slots/slot">
                  <xsl:for-each select="direct-slots/slot">
                    <xsl:sort select="@id" data-type="text" order="ascending"/>
                    <xsl:call-template name="slot-list"/>
                  </xsl:for-each>
                </xsl:when>
                <xsl:otherwise>
                  None
                </xsl:otherwise>
              </xsl:choose>
            </xsl:if>
            <xsl:call-template name="main-documentation-string"/>
            <div id="version">
              <xsl:apply-templates select="version"/>
            </div>
          </div>
        </div>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="built-in-class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Built-in Class'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>

            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>

        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="condition-class-definition">
    <xsl:call-template name="definition">
      <xsl:with-param name="label" select="'Condition Type'"/>
    </xsl:call-template>
    <div class="sp-definition">
      <div class="sp-definition-body">
        <div class="sph3">Superclasses:</div>
        <div class="indent">
          <xsl:for-each select="cpl/superclass">
            <xsl:call-template name="class-list"/>
          </xsl:for-each>
        </div>
        <div class="sph3">Documented Subclasses:</div>
        <div class="indent">
          <xsl:choose>
            <xsl:when test="subclasses/subclass">
              <xsl:for-each select="subclasses/subclass">
                <xsl:sort select="@id" data-type="text" order="ascending"/>
                <xsl:call-template name="class-list"/>
              </xsl:for-each>
            </xsl:when>
            <xsl:otherwise>
              None
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <xsl:if test="direct-slots">
          <div class="sph3">Direct Slots:</div>

            <xsl:choose>
              <xsl:when test="direct-slots/slot">
                <xsl:for-each select="direct-slots/slot">
                  <xsl:sort select="@id" data-type="text" order="ascending"/>
                  <xsl:call-template name="slot-list"/>
                </xsl:for-each>
              </xsl:when>
              <xsl:otherwise>
                None
              </xsl:otherwise>
            </xsl:choose>

        </xsl:if>
        <xsl:call-template name="main-documentation-string"/>
        <div id="version">
          <xsl:apply-templates select="version"/>
        </div>
      </div>
    </div>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>&#160;</xsl:text>
      <span style="color: red">
        (undocumented)
      </span>
    </xsl:if>
  </xsl:template>

<!--
  <xsl:template match="package">
    <xsl:apply-templates select="sections/section"/>
    <xsl:variable name="unreferenced"
                  select="external-symbols/function-definition[
                          count(key('about-function',@name))=0]"/>
    <xsl:if test="$unreferenced">
      <h3>Other Functions in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
-->

<!--
    <xsl:variable name="unreferenced6"
                  select="external-symbols/symbol-definition[
                          count(key('about-symbol',@name))=0]"/>
    <xsl:if test="$unreferenced6">
      <h3>Other Symbols in <xsl:value-of select="@name"/></h3>
      <xsl:apply-templates select="$unreferenced6">
        <xsl:sort select="@id" data-type="text" order="ascending"/>
      </xsl:apply-templates>
    </xsl:if>
-->


</xsl:stylesheet>
