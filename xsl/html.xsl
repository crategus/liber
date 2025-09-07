<!-- Hey, emacs, please consider this to be -*- xml -*-
    This is the main stylesheet.
    Input must have been cleaned up using cleanup.xsl already.

    This stylesheet does nearly all of the formatting work, but still keeps
    all data together in one big XML document.

    A <page> element is produced for each package and symbol.

    The contents of each <page> will be mostly HTML, with the exception
    of a few formatting elements like <columns> that are replaced later.

  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:macro="http://crategus.com/macro"
                version="1.0">

  <xsl:import href="html-common.tmp"/>
  <xsl:include href="base-uri.xsl"/>
  <xsl:output method="xml" indent="yes"/>

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
           match="gobject-class-definition"
           use="@id"/>
  <xsl:key name="standard-class-by-name"
           match="standard-class-definition"
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
      <xsl:call-template name="configuration-attributes"/>
      <xsl:apply-templates select="documentation"/>
      <xsl:apply-templates select="documentation/package"/>
      <xsl:apply-templates select="documentation/package/external-symbols/*"/>
      <xsl:if test="documentation/@include-internal-symbols-p">
        <xsl:apply-templates select="documentation/package/internal-symbols/*"/>
      </xsl:if>
    </pages>
  </xsl:template>

  <xsl:template name="configuration-attributes">
    <macro:copy-attribute name="logo" path="documentation"/>
    <macro:copy-attribute name="css" path="documentation"/>
    <macro:copy-attribute name="heading" path="documentation"/>
    <macro:copy-attribute name="ico" path="documentation"/>
  </xsl:template>

  <!--
      page generation templates
    -->

  <xsl:template match="documentation">
    <main-page title="{@index-title}"
               author="{/documentation/@author}"
               author-url="{/documentation/@author-url}"
               date="{/documentation/@date}"
               keywords="Lisp, Documentation, Package, {@name}">
      <padded>
        Index of packages:
      </padded>
      <columns>
        <column width="60%">
          <padded>
            <xsl:for-each select="package">
              <xsl:variable name="url"
                            select="concat('pages/', @id, '.html')"/>
              <h2 class="page-title">
                <a href="{$url}">
                  Package
                  <xsl:value-of select="@name"/>
                </a>
              </h2>
              <div style="left: 100px">
                <xsl:apply-templates select="documentation-string"/>
                <div class="indent">
                  <xsl:if test="sections">
                    <p><i>About this package:</i></p>
                    <ul>
                      <xsl:for-each select="sections/section">
                        <li>
                          <xsl:choose>
                            <xsl:when test="/documentation/@paginate-section-p">
                              <a href="{concat('pages/', @id, '.html')}">
                                <xsl:value-of select="@section"/>
                              </a>
                            </xsl:when>
                            <xsl:otherwise>
                              <a href="{$url}#{generate-id()}">
                                <xsl:value-of select="@section"/>
                              </a>
                            </xsl:otherwise>
                          </xsl:choose>
                        </li>
                      </xsl:for-each>
                    </ul>
                  </xsl:if>
                </div>
              </div>
            </xsl:for-each>
          </padded>
        </column>
        <column width="40%">
          <h3 id="index">Exported Symbol Index</h3>
          <simple-table>
            <xsl:apply-templates select="package/external-symbols/*"
                                 mode="symbol-index">
              <xsl:sort select="@id" data-type="text" order="ascending"/>
              <xsl:with-param name="packagep" select="'pages/'"/>
            </xsl:apply-templates>
          </simple-table>
        </column>
      </columns>
    </main-page>
  </xsl:template>

  <xsl:template match="package">
    <xsl:choose>
      <xsl:when test="/documentation/@paginate-section-p">
        <!-- Generate the first page: Content -->
        <page base="../"
              pathname="pages/{@id}.html"
              title="Package {@name} - {/documentation/@index-title}"
              author="{/documentation/@author}"
              author-url="{/documentation/@author-url}"
              date="{/documentation/@date}"
              keywords="Lisp, Documentation, Package, {@name}">
          <padded>
            <xsl:if test="count(../package) > 1">
              <p class="noindent">
                Up:
                <a href="../index.html">
                  <xsl:value-of select="/documentation/@index-title"/>
                </a>
              </p>
            </xsl:if>
            <h1>
              Package
              <xsl:value-of select="@name"/>
            </h1>
            <xsl:apply-templates select="documentation-string"/>
          </padded>
          <columns>
            <column width="60%">
              <padded>
                <xsl:if test="sections">
                  <div style="margin-left: -30px">
                    <h3>About This Package</h3>
                  </div>
                  <xsl:for-each select="sections/section">
                    <a href="{@id}.html" style="font-weight: bold">
                      <xsl:value-of select="@section"/>
                    </a>
                    <br/>
                  </xsl:for-each>
                  <br/>
                </xsl:if>
              </padded>
            </column>
            <column width="40%">
              <h3 id="index">Exported Symbol Index</h3>
              <xsl:apply-templates select="external-symbols" mode="symbol-index"/>
            </column>
          </columns>
        </page>
        <!-- Now generate a page for each section -->
        <xsl:for-each select="sections/section">
          <page base="../"
                pathname="pages/{@id}.html"
                title="Package {package/@name} - {/documentation/@index-title}"
                author="{/documentation/@author}"
                author-url="{/documentation/@author-url}"
                date="{/documentation/@date}"
                keywords="Lisp, Documentation, Package, {@name}">
            <xsl:variable name="in-section" select="@section"/>
            <padded>
              <p class="noindent">
                Package:
                <a href= "{../../@id}.html">
                  <xsl:value-of select="../../@name"/>
                </a>
                - Section:
                <xsl:value-of select="@section"/>
              </p>
              <h1>
                Package
                <xsl:value-of select="../../@name"/>
              </h1>
              <xsl:apply-templates select="../../documentation-string"/>
            </padded>
            <columns>
              <column width="60%">
                <padded>
                  <xsl:if test="../../sections">
                    <div style="margin-left: -30px">
                      <h3>About This Package</h3>
                    </div>
                    <xsl:for-each select="../../sections/section">
                      <xsl:choose>
                        <xsl:when test="@section = $in-section">
                          <xsl:value-of select="@section"/>
                        </xsl:when>
                        <xsl:otherwise>
                          <a href="{@id}.html" style="font-weight: bold">
                            <xsl:value-of select="@section"/>
                          </a>
                        </xsl:otherwise>
                      </xsl:choose>
                      <br/>
                    </xsl:for-each>
                    <br/>
                    <h2 id="{@id}.html">
                      <xsl:value-of select="@section"/>
                    </h2>
                    <xsl:apply-templates/>
                  </xsl:if>
                </padded>
              </column>
              <column width="40%">
                <h3 id="index">Exported Symbol Index</h3>
                <xsl:apply-templates select="../../external-symbols" mode="symbol-index"/>
              </column>
            </columns>
          </page>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <page base="../"
              pathname="pages/{@id}.html"
              title="Package {@name} - {/documentation/@index-title}"
              author="{/documentation/@author}"
              author-url="{/documentation/@author-url}"
              date="{/documentation/@date}"
              keywords="Lisp, Documentation, Package, {@name}">
          <padded>
            <xsl:if test="count(../package) > 1">
              <p class="noindent">
                Up:
                <a href="../index.html">
                  <xsl:value-of select="/documentation/@index-title"/>
                </a>
              </p>
            </xsl:if>
            <h1>
              Package
              <xsl:value-of select="@name"/>
            </h1>
            <xsl:apply-templates select="documentation-string"/>
          </padded>
          <columns>
            <column width="60%">
              <padded>
                <xsl:if test="sections">
                  <div style="margin-left: -30px">
                    <h3>About This Package</h3>
                  </div>
                  <xsl:for-each select="sections/section">
                    <a href="#{generate-id()}" style="font-weight: bold">
                      <xsl:value-of select="@section"/>
                    </a>
                    <br/>
                  </xsl:for-each>
                  <br/>
                  <xsl:apply-templates select="sections"/>
                </xsl:if>
              </padded>
            </column>
            <column width="40%">
              <h3 id="index">Exported Symbol Index</h3>
              <xsl:apply-templates select="external-symbols" mode="symbol-index"/>
            </column>
          </columns>
        </page>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="sections">
    <xsl:for-each select="section">
      <h2 id="{generate-id()}">
        <xsl:value-of select="@section"/>
      </h2>
      <xsl:apply-templates/>
    </xsl:for-each>
  </xsl:template>

<!--
  <xsl:template match="sections">
    <xsl:for-each select="section">
      <h2>
        <a name="@id"/>
        <xsl:value-of select="@section"/>
      </h2>
      <xsl:apply-templates select="section-page"/>
    </xsl:for-each>
  </xsl:template>
-->

  <xsl:template match="section-page">
    <page base="../"
          pathname="pages/{@id}.html"
          title="Section {@section} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, Package, {@section}">
      <padded>
        <xsl:if test="count(../package/sections/section) > 1">
          <p class="noindent">
            Up:
            <a href="../index.html">
              <xsl:value-of select="/documentation/@index-title"/>
            </a>
          </p>
        </xsl:if>
        <h1>
          Package
          <xsl:value-of select="@name"/>
        </h1>
        <xsl:apply-templates select="documentation-string"/>
      </padded>
      <columns>
        <column width="60%">
          <padded>
            <xsl:if test="sections">
              <div style="margin-left: -30px">
                <h3>About This Package</h3>
              </div>
              <xsl:for-each select="sections/section">
                <a href="#{generate-id()}" style="font-weight: bold">
                  <xsl:value-of select="@section"/>
                </a>
                <br/>
              </xsl:for-each>
                <br/>
                <xsl:apply-templates/>
            </xsl:if>
          </padded>
        </column>
        <column width= "40%">
<!--          <h3><a name="index"></a>Exported Symbol Index</h3>  -->
          <h3 id="index">Exported Symbol Index</h3>
          <xsl:apply-templates select="external-symbols" mode="symbol-index"/>
        </column>
      </columns>
    </page>
  </xsl:template>

  <xsl:template name="main-left">
    <xsl:choose>
      <xsl:when test="documentation-string">
        <h3>Details</h3>
        <xsl:apply-templates select="documentation-string"/>
      </xsl:when>
      <xsl:otherwise>
        <p style="color: red; font-weight: bold">
          No documentation string.  Possibly unimplemented or incomplete.
        </p>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:apply-templates select="implementation-note"/>
    <xsl:apply-templates select="note"/>
  </xsl:template>

  <xsl:template name="main-right">
    <xsl:if test="see-also/constructor">
      <h3>Returned by</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates select="see-also/constructor/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/condition">
      <h3>Condition Types Signalled</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates select="see-also/condition/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/slot">
      <h3>Slot Access Functions</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates select="see-also/slot/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if
       test="key('id', .//superclass/@id)//see-also/slot">
      <h3>Inherited Slot Access Functions</h3>
      <div class="indent">
        <simple-table>
          <xsl:apply-templates
              select="key('id', .//superclass/@id)//see-also/slot/see"/>
        </simple-table>
      </div>
    </xsl:if>
    <xsl:if test="see-also/other|see-also/auto">
      <h3>See also</h3>
      <div class="indent">
        <!-- TODO: Implement an attribute nowrap for a list of functions -->
        <simple-table nowrap="nowrap">
          <xsl:for-each select="see-also/other/see|see-also/auto/see">
            <xsl:variable name="name" select="text()"/>
            <xsl:if test="not(preceding-sibling::see[text() = $name])">
              <xsl:apply-templates select="."/>
            </xsl:if>
          </xsl:for-each>
        </simple-table>
      </div>
    </xsl:if>
  </xsl:template>

  <xsl:template name="main">
    <macro:maybe-columns test="see-also">
      <xsl:call-template name="main-left"/>
    </macro:maybe-columns>
  </xsl:template>

<!-- Pages for the definitions of the external symbols -->

  <xsl:template match="symbol-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Common Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <xsl:apply-templates select="declaration"/>
          <xsl:apply-templates select="values"/>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="valid-context"/>
          <xsl:apply-templates select="binding-types"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="variable-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <xsl:apply-templates select="variable-type"/>
          <xsl:apply-templates select="variable-value"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="function-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="generic-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="operator-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="macro-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
           <xsl:value-of select="../../@name"/>
         </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <h3>Lambda List</h3>
          <div class="indent">
            <xsl:apply-templates select="lambda-list"/>
          </div>
          <xsl:apply-templates select="syntax"/>
          <xsl:apply-templates select="arguments"/>
          <xsl:apply-templates select="return"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="type-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns test="see-also">
        <padded>
          <xsl:apply-templates select="declaration"/>
          <xsl:apply-templates select="values"/>
          <xsl:apply-templates select="supertypes"/>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="gobject-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
        <div>
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
        </div>
      </xsl:if>
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="standard-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns
          test="see-also or key('id', .//superclass/@id)//see-also/slot">
        <xsl:choose>
          <!-- Template for the standard class with kindname GBoxed -->
          <xsl:when test="@kindname='GBoxed'">
            <padded>
              <xsl:apply-templates select="declaration"/>
              <xsl:apply-templates select="values"/>
              <xsl:apply-templates select="syntax"/>
              <xsl:apply-templates select="arguments"/>
              <xsl:apply-templates select="valid-context"/>
              <xsl:apply-templates select="binding-types"/>
              <xsl:call-template name="main-left"/>
            </padded>
          </xsl:when>
          <!-- Template for the standard class -->
          <xsl:otherwise>
            <padded>
              <h3>Superclasses</h3>
              <div class="indent">
                <xsl:for-each select="cpl/superclass">
                  <xsl:call-template name="class-list"/>
                </xsl:for-each>
              </div>
              <h3>Documented Subclasses</h3>
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
                <h3>Direct Slots</h3>
                <div>
                  <xsl:choose>
                    <xsl:when test="direct-slots/slot">
                      <xsl:for-each select="direct-slots/slot">
                        <xsl:sort select="@id"
                                  data-type="text" order="ascending"/>
                        <xsl:call-template name="slot-list"/>
                      </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
                      <div class="indent">
                        None
                      </div>
                    </xsl:otherwise>
                  </xsl:choose>
                </div>
              </xsl:if>
              <xsl:call-template name="main-left"/>
            </padded>
          </xsl:otherwise>
        </xsl:choose>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

<!-- Das Original
  <xsl:template match="standard-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
        <div>
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
        </div>
      </xsl:if>
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>
-->

  <xsl:template match="system-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}"
          title="{@kindname} {@name} - {/documentation/@index-title}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns
       test="see-also or key('id', .//superclass/@id)//see-also/slot">
        <padded>
          <h3>Superclasses</h3>
          <div class="indent">
            <xsl:for-each select="cpl/superclass">
              <xsl:call-template name="class-list"/>
            </xsl:for-each>
          </div>
          <h3>Documented Subclasses</h3>
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
            <h3>Direct Slots</h3>
            <div class="indent">
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
            </div>
          </xsl:if>
          <xsl:call-template name="main-left"/>
        </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="structure-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
        <p class="noindent">
          Package:
          <a href="{../../@id}.html">
            <xsl:value-of select="../../@name"/>
          </a>
        </p>
        <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </h2>
      </padded>
      <macro:maybe-columns
          test="see-also or key('id', .//superclass/@id)//see-also/slot">
        <xsl:choose>
          <!-- Template for the structure class with kindname GBoxed -->
          <xsl:when test="@kindname='GBoxed'">
            <padded>
              <xsl:apply-templates select="declaration"/>
              <xsl:apply-templates select="values"/>
              <xsl:apply-templates select="syntax"/>
              <xsl:apply-templates select="arguments"/>
              <xsl:apply-templates select="valid-context"/>
              <xsl:apply-templates select="binding-types"/>
              <xsl:call-template name="main-left"/>
            </padded>
          </xsl:when>
          <!-- Template for the structure class -->
          <xsl:otherwise>
            <padded>
              <h3>Superclasses</h3>
              <div class="indent">
                <xsl:for-each select="cpl/superclass">
                  <xsl:call-template name="class-list"/>
                </xsl:for-each>
              </div>
              <h3>Documented Subclasses</h3>
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
                <h3>Direct Slots</h3>
                <div class="indent">
                  <xsl:choose>
                    <xsl:when test="direct-slots/slot">
                      <xsl:for-each select="direct-slots/slot">
                        <xsl:sort select="@id"
                                  data-type="text" order="ascending"/>
                        <xsl:call-template name="slot-list"/>
                      </xsl:for-each>
                    </xsl:when>
                    <xsl:otherwise>
                      <div class="indent">
                        None
                      </div>
                    </xsl:otherwise>
                  </xsl:choose>
                </div>
              </xsl:if>
              <xsl:call-template name="main-left"/>
            </padded>
          </xsl:otherwise>
        </xsl:choose>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

<!-- Das Original
  <xsl:template match="structure-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
        <div class="indent">
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
        </div>
      </xsl:if>
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>
-->
  <xsl:template match="built-in-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
        <div class="indent">
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
        </div>
      </xsl:if>
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <xsl:template match="condition-class-definition">
    <page base="../"
          pathname="pages/{@id}.html"
          title="{@kindname} {@name} - {/documentation/@index-title}"
          author="{/documentation/@author}"
          author-url="{/documentation/@author-url}"
          date="{/documentation/@date}"
          keywords="Lisp, Documentation, {@package}, {@kindname}, {@name}">
      <padded>
    <p class="noindent">
      Package:
      <a href="{../../@id}.html">
        <xsl:value-of select="../../@name"/>
      </a>
    </p>
    <h2 class="page-title">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="../../@name"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
    </h2>
      </padded>
      <macro:maybe-columns
     test="see-also or key('id', .//superclass/@id)//see-also/slot">
    <padded>
      <h3>Superclasses</h3>
      <div class="indent">
        <xsl:for-each select="cpl/superclass">
          <xsl:call-template name="class-list"/>
        </xsl:for-each>
      </div>
      <h3>Documented Subclasses</h3>
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
        <h3>Direct Slots</h3>
        <div class="indent">
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
        </div>
      </xsl:if>
      <xsl:call-template name="main-left"/>
    </padded>
      </macro:maybe-columns>
      <xsl:apply-templates select="version"/>
    </page>
  </xsl:template>

  <!--
      Other templates
    -->

  <xsl:template name="class-list">
    <xsl:if test="position() != 1">
      <xsl:text>, </xsl:text>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
    <span class="sym">
      <xsl:value-of select="../@package"/>
      <xsl:text>:</xsl:text>
      <xsl:value-of select="../@name"/>
    </span>
    <xsl:text> (</xsl:text>
    <xsl:for-each select="elt">
      <xsl:if test="position() != 1">
        <!-- put a space between the arguments -->
        <xsl:text>&#160;</xsl:text>

     <!-- a workaround for very long lambda-lists
          put in a linebreak after 9 symbols -->
      <xsl:if test="position() = 10">
        <xsl:text>&#xa;</xsl:text>
      </xsl:if>

      </xsl:if>
      <b><xsl:value-of select="text()"/></b>
    </xsl:for-each>
    <xsl:text>)</xsl:text>
  </xsl:template>

  <xsl:template name="slot-list">
    <div class="indent">
      <simple-table>
<!--        <a name="{@id}"/> -->
<!--        <a id="{@id}">    -->
        <span id="{@id}" class="code">
          <xsl:value-of select="@name"/>
        </span>

        <xsl:apply-templates select="documentation-string"/>
<!--
        <xsl:choose>
          <xsl:when test="documentation-string/short">
            <xsl:apply-templates select="documentation-string/short"/>
            <a href="{@id}.html#details">...</a>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates
              select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
-->
      </simple-table>
    </div>
  </xsl:template>

  <xsl:template name="about-arguments">
    <div class="def">
      <a href="{../@id}.html">
        <xsl:value-of select="../@kindname"/>
        <xsl:text> </xsl:text>

        <xsl:value-of select="../@package"/>
        <xsl:text>:</xsl:text>

        <xsl:value-of select="../@name"/>
        <xsl:text> (</xsl:text>
        <xsl:for-each select="elt">
          <xsl:if test="position() != 1">
            <xsl:text>&#160;</xsl:text>
          </xsl:if>

     <!-- a workaround for very long lambda-lists
          put in a linebreak after 9 symbols -->
      <xsl:if test="position() = 10">
        <xsl:text>&#xa;</xsl:text>
      </xsl:if>


          <xsl:value-of select="text()"/>
        </xsl:for-each>
        <xsl:text>)</xsl:text>
      </a>
    </div>
  </xsl:template>

  <xsl:template match="documentation-string">
    <div class="indent">
      <xsl:apply-templates/>
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


  <xsl:template match="var">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

<!--
  <xsl:template match="fun">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>
-->

  <xsl:template match="fun">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="gen">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="macro">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="type">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

<!-- Replace with the short name sym -->
  <xsl:template match="symbol">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="sym">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="val">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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

  <xsl:template match="operator">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="class">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="struct">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@id}.html">
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
  </xsl:template>

  <xsl:template match="condition">
    <a href="{@id}.html">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
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
    <tr>
      <td>
<!--
        <a href="{@id}.html">
          <span class="code">
            <xsl:apply-templates/>
          </span>
        </a>
-->
        <xsl:choose>
          <xsl:when test="@id">
            <a href="{@id}.html">
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

      </td>
      <xsl:if test="@see">
        <td>
          &#160;&#160;&#160;&#160;
          <i>
            <xsl:value-of select="@see"/>
          </i>
        </td>
      </xsl:if>
    </tr>
  </xsl:template>

  <xsl:template match="return">
    <h3>Return Value</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="heading">
    <div class="noindent">
    <h4><xsl:apply-templates/></h4>
    </div>
  </xsl:template>

  <xsl:template match="subheading">
    <h5><xsl:apply-templates/></h5>
  </xsl:template>

  <xsl:template match="implementation-note">
    <h3>Implementation notes</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="note">
    <h3>Notes</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <!-- Dictionary Entries -->

  <xsl:template match="dictionary">
    <div class="noindent">
      <h3>
        <xsl:value-of select="@dictionary"/>
      </h3>
    </div>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="declaration">
      <h3>Declaration</h3>
      <div class="indent">
        <pre>
          <xsl:apply-templates/>
        </pre>
      </div>
  </xsl:template>

  <xsl:template match="values">
      <h3>Values</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
  </xsl:template>

<!-- Improve this to include ID for references -->

  <xsl:template match="signal">
    <div id="{@id}" class="signal">
      <h5>
        The "<xsl:value-of select="@signal"/>" signal
      </h5>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="sig">
    <xsl:choose>
      <xsl:when test="@id">
        <span>"<a href="{@classid}.html#{@id}">
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
        <span><a href="{@classid}.html#{@id}">
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

  <xsl:template match="variable-type">
    <h3>Value Type</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="variable-value">
    <h3>Value</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="see-also">
    <div class="noindent">
      <h3>See Also</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="supertypes">
    <h3>Supertypes</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="version">
    <div id="version">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="valid-context">
    <h3>Valid Context</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

  <xsl:template match="binding-types">
    <h3>Binding Types Affected</h3>
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

<!--
  <xsl:template match="syntax">
    <h3>Syntax</h3>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="syntax">
          <xsl:choose>
            <xsl:when test="not(@syntax='')">
              <span class="code">
                <xsl:value-of select="@syntax"/>
                <xsl:text> ::= </xsl:text>
                <xsl:apply-templates/>
              </span>
            </xsl:when>
            <xsl:otherwise>
              <span class="code">
                <xsl:apply-templates/>
              </span>
            </xsl:otherwise>
          </xsl:choose>
          <br/>
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>
-->

  <xsl:template match="syntax">
    <h3>Syntax</h3>
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

  <xsl:template match="arguments">
    <h3>Arguments</h3>
    <div class="indent">
      <simple-table>
        <xsl:for-each select="argument">
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
        </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>

<!--
  <xsl:template match="arguments">
    <h3>Arguments</h3>
    <div class="indent">
      <simple-table>
    <xsl:for-each select="argument">
      <span class="arg">
        <xsl:value-of select="@argument"/>
      </span>
      <xsl:text>  </xsl:text>
      <xsl:apply-templates/>
      <br/>
    </xsl:for-each>
      </simple-table>
    </div>
  </xsl:template>
-->

  <xsl:template match="affected-by">
    <div class="noindent">
      <h3>Affected By</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="examples">
    <div class="noindent">
      <h3>Examples</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="exceptional">
    <div class="noindent">
      <h3>Exceptional Situations</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="notes">
    <div class="noindent">
      <h3>Notes</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="subsection">
    <div class="noindent">
      <br/>
      <h4 id="{@id}">
<!--        <a name="{@id}"/> -->
        <xsl:value-of select="@subsection"/>
      </h4>

      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="subsubsection">
    <div class="noindent">
      <h5><xsl:value-of select="@subsubsection"/></h5>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <xsl:template match="methods">
    <div class="noindent">
      <h3>Methods</h3>
      <div class="indent">
        <xsl:apply-templates/>
      </div>
    </div>
  </xsl:template>

  <!-- End of Dictionary entries -->

  <xsl:template match="slot">
    <xsl:choose>
      <xsl:when test="@id">
        <a href="{@classid}.html#{@id}">
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

  <xsl:template match="about-symbol">
    <xsl:for-each select="key('symbol-by-name', @id)">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
          </xsl:when>
            <xsl:otherwise>
            <xsl:apply-templates select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-variable">
    <xsl:for-each select="key('variable-by-name', @id)">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-function">
    <xsl:for-each select="key('function-by-name', @id)">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Function'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-generic">
    <xsl:for-each select="key('generic-by-name', @id)">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Generic Function'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-operator">
    <xsl:for-each select="key('operator-by-name', @id)">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Special Operator'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-macro">
    <xsl:for-each select="key('macro-by-name', @id)">
      <xsl:for-each select="lambda-list">
        <xsl:call-template name="about-arguments">
          <xsl:with-param name="label" select="'Macro'"/>
        </xsl:call-template>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-type">
    <xsl:for-each select="key('type-by-name', @id)">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
          </xsl:when>
            <xsl:otherwise>
            <xsl:apply-templates select="documentation-string"/>
          </xsl:otherwise>
        </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-class">
    <xsl:for-each select="key('standard-class-by-name', @id)">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-struct">
    <xsl:for-each select="key('structure-class-by-name', @id)">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="about-condition">
    <xsl:for-each select="key('condition-class-by-name', @id)">
      <div class="def">
        <a href="{@id}.html">
          <xsl:value-of select="@kindname"/>
          <xsl:text> </xsl:text>
          <xsl:value-of select="@package"/>
          <xsl:text>:</xsl:text>
          <xsl:value-of select="@name"/>
        </a>
      </div>
      <xsl:choose>
        <xsl:when test="documentation-string//short">
          <div class="indent">
            <xsl:apply-templates select="documentation-string//short"/>
            <xsl:text> </xsl:text>
            <a href="{@id}.html#details">...</a>
          </div>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="documentation-string"/>
        </xsl:otherwise>
      </xsl:choose>
      <br/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="undocumented">
    <xsl:if test="not(documentation-string)">
      <xsl:text>&#160;</xsl:text>
      <span style="color: red">
        (undocumented)
      </span>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
