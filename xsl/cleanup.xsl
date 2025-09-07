<!--  hey, emacs, treat this file as -*- xml -*-, please
    The first stylesheet to be used, this file cleans up the structure
    of the Lisp-generated XML file by extracting elements from all
    docstrings into their parent elements, so that only the textual
    description remains in the <documentation-string>.

    Example (input):

    <class-definition name="foo">
      <cpl>...</cpl>

      <documentation-string>
        The foo class.
        <see-slot id="foo">See also the foo function.</see-slot>
        Beware bugs.
      </documentation-string>
    </class-definition>

    Output:

    <class-definition name="foo">
      <cpl>...</cpl>

      <see-also>
        <slot>
          <see id="foo">See also the foo function.</see>
        </slot>
      </see-also>

      <documentation-string>
        The foo class.
        Beware bugs.
      </documentation-string>
    </class-definition>

  -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:include href="base-uri.xsl"/>
  <xsl:output method="xml" indent="yes"/>

  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="/*">
    <xsl:copy>
      <xsl:call-template name="copy-base-uri"/>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="see"/>

  <!-- Reference of a Lisp symbol -->
  <xsl:template match="see-symbol"/>
  <xsl:template match="see-type"/>
  <xsl:template match="see-variable"/>
  <xsl:template match="see-function"/>
  <xsl:template match="see-macro"/>
  <xsl:template match="see-generic"/>
  <xsl:template match="see-operator"/>
  <xsl:template match="see-class"/>
  <xsl:template match="see-slot"/>
  <xsl:template match="see-struct"/>
  <xsl:template match="see-condition"/>

  <xsl:template match="see-constructor"/>
  <xsl:template match="see-signalled"/>
  <xsl:template match="see-section"/>

  <!-- Entries for function definitions -->
  <xsl:template match="syntax"/>
  <xsl:template match="argument"/>
  <xsl:template match="return"/>
  <xsl:template match="declaration"/>
  <xsl:template match="values"/>
  <xsl:template match="note"/>


  <xsl:template match="implementation-note"/>
  <xsl:template match="variable-type"/>
  <xsl:template match="variable-value"/>
  <xsl:template match="supertypes"/>
  <xsl:template match="valid-context"/>
  <xsl:template match="binding-types"/>

  <xsl:template match="version"/>
  <xsl:template match="section"/>

  <xsl:template mode="extract" match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template mode="extract" match="see-symbol">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-type">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-variable">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-slot">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>


  <xsl:template mode="extract" match="see-function">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-generic">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-operator">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-macro">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-class">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-struct">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-condition">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-signalled">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-constructor">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template mode="extract" match="see-section">
    <see>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates/>
    </see>
  </xsl:template>

  <xsl:template match="symbol-definition |
                       type-definition |
                       variable-definition |
                       macro-definition |
                       operator-definition |
                       generic-definition |
                       function-definition |
                       structure-class-definition |
                       standard-class-definition |
                       system-class-definition |
                       built-in-class-definition |
                       condition-class-definition |
                       gobject-class-definition">
    <xsl:if test="not(.//unexport)">
      <xsl:copy>
        <xsl:apply-templates select="@*|node()"/>
      </xsl:copy>
    </xsl:if>
  </xsl:template>

  <xsl:template match="documentation-string">
    <xsl:if test=".//syntax">
      <syntax>
        <xsl:apply-templates mode="extract" select=".//syntax"/>
      </syntax>
    </xsl:if>

    <xsl:if test=".//argument">
      <arguments>
        <xsl:apply-templates mode="extract" select=".//argument"/>
      </arguments>
    </xsl:if>

    <xsl:if test=".//section">
      <sections>
        <xsl:apply-templates mode="extract" select=".//section"/>
      </sections>
    </xsl:if>

    <xsl:if test=".//see or .//see-symbol
                         or .//see-type
                         or .//see-variable
                         or .//see-slot
                         or .//see-constructor
                         or .//see-function
                         or .//see-generic
                         or .//see-operator
                         or .//see-macro
                         or .//see-class
                         or .//see-struct
                         or .//see-condition
                         or .//see-signalled
                         or .//see-section">
      <see-also>
        <xsl:if test=".//see or .//see-symbol
                             or .//see-type
                             or .//see-variable
                             or .//see-function
                             or .//see-generic
                             or .//see-operator
                             or .//see-macro
                             or .//see-class
                             or .//see-struct
                             or .//see-condition">
          <other>
            <xsl:apply-templates mode="extract"
                                 select=".//see |
                                         .//see-symbol |
                                         .//see-type |
                                         .//see-variable |
                                         .//see-function |
                                         .//see-generic |
                                         .//see-operator |
                                         .//see-macro |
                                         .//see-class |
                                         .//see-struct |
                                         .//see-condition"/>
          </other>
        </xsl:if>

        <xsl:if test=".//see-slot">
          <slot>
            <xsl:apply-templates mode="extract" select=".//see-slot"/>
          </slot>
        </xsl:if>

        <xsl:if test=".//see-constructor">
          <constructor>
            <xsl:apply-templates mode="extract" select=".//see-constructor"/>
          </constructor>
        </xsl:if>

        <xsl:if test=".//see-signalled">
          <condition>
            <xsl:apply-templates mode="extract" select=".//see-signalled"/>
          </condition>
        </xsl:if>

        <xsl:if test=".//see-section">
          <reference>
            <xsl:apply-templates mode="extract" select=".//see-section"/>
          </reference>
        </xsl:if>

      </see-also>
    </xsl:if>

    <xsl:apply-templates mode="extract" select=".//variable-type"/>
    <xsl:apply-templates mode="extract" select=".//variable-value"/>
    <xsl:apply-templates mode="extract" select=".//supertypes"/>
    <xsl:apply-templates mode="extract" select=".//version"/>
    <xsl:apply-templates mode="extract" select=".//valid-context"/>
    <xsl:apply-templates mode="extract" select=".//binding-types"/>
    <xsl:apply-templates mode="extract" select=".//implementation-note"/>
    <xsl:apply-templates mode="extract" select=".//note"/>
    <xsl:apply-templates mode="extract" select=".//return"/>
    <xsl:apply-templates mode="extract" select=".//declaration"/>
    <xsl:apply-templates mode="extract" select=".//values"/>

    <documentation-string>
      <xsl:apply-templates/>
    </documentation-string>
  </xsl:template>
</xsl:stylesheet>
