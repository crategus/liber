<!-- Hey, emacs, please consider this to be -*- xml -*-
  -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:macro="http://crategus.com/macro"
                version="1.0">

  <!-- Symbol index -->

  <xsl:template match="*" mode="symbol-index"/>

  <xsl:template match="external-symbols" mode="symbol-index">
    <xsl:param name="packagep"/>
    <simple-table>
      <xsl:apply-templates mode="symbol-index">

<!--        <xsl:sort select="@name" data-type="text" order="ascending"/> -->

        <xsl:sort select="@sortid" data-type="text" order="ascending"/>

        <xsl:with-param name="packagep" select="$packagep"/>
      </xsl:apply-templates>
    </simple-table>
  </xsl:template>

  <xsl:template name="index-entry">
    <xsl:param name="packagep"/>
    <xsl:param name="kind"/>
    <row>
      <xsl:if test="$packagep">
        <cell align="right" nowrap="nowrap">
          <span class="nonlink">
            <tt>
              <span style="color: #777777">
                <xsl:value-of select="../../@name"/>
                <xsl:text>:</xsl:text>
              </span>
            </tt>
          </span>
        </cell>
      </xsl:if>
      <cell>
        <a>
          <xsl:choose>
            <xsl:when test="/documentation/@single-page-p">
              <xsl:attribute name="href">
                <xsl:text>#</xsl:text>
                <xsl:value-of select="@id"/>
              </xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="href">
                <xsl:value-of select="$packagep"/>
                <xsl:value-of select="@id"/>
                <xsl:text>.html</xsl:text>
              </xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <span class="code">
            <xsl:value-of select="@name"/>
          </span>
        </a>
        <xsl:text>, </xsl:text>
        <xsl:value-of select="@kindname"/>
        <xsl:call-template name="undocumented"/>
      </cell>
    </row>
  </xsl:template>

  <xsl:template match="symbol-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'symbol'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="type-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'type'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="variable-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'variable'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="function-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'function'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="generic-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'generic function'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="operator-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'special operator'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="macro-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'macro'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="gobject-class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'class'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="standard-class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'class'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="system-class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'system-class'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="structure-class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'struct'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="built-in-class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'built-in-class'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="condition-class-definition" mode="symbol-index">
    <xsl:param name="packagep"/>
    <xsl:call-template name="index-entry">
      <xsl:with-param name="packagep" select="$packagep"/>
      <xsl:with-param name="kind" select="'condition'"/>
    </xsl:call-template>
  </xsl:template>

  <!-- General Fonts -->

  <xsl:template match="em">
    <i>
      <xsl:apply-templates/>
    </i>
  </xsl:template>

  <xsl:template match="b">
    <b>
      <xsl:apply-templates/>
    </b>
  </xsl:template>

  <xsl:template match="u">
    <u>
      <xsl:apply-templates/>
    </u>
  </xsl:template>

  <xsl:template match="kbd">
    <span class="kbd">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <!-- Fonts Used in the Common Lisp Hyperspec -->

  <xsl:template match="term">
    <span class="term">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="defterm">
    <span class="defterm">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

<!-- Replaced for references to symbols
  Remove older usage from all files
  <xsl:template match="sym">
    <span class="sym">
      <xsl:apply-templates/>
    </span>
  </xsl:template>
-->

  <xsl:template match="code">
    <span class="code">
      <xsl:apply-templates/>
    </span>
  </xsl:template>

  <xsl:template match="arg">
    <span class="arg">
        <xsl:apply-templates/>
    </span>
  </xsl:template>

<!-- Formating of text -->

  <xsl:template match="file">
    <tt><em>
      <xsl:apply-templates/>
    </em></tt>
  </xsl:template>

  <xsl:template match="break">
    <br/><br/>
  </xsl:template>

  <xsl:template match="br">
      <br/>
  </xsl:template>

  <xsl:template match="pre">
    <pre>
      <xsl:apply-templates/>
    </pre>
  </xsl:template>

  <xsl:template match="indent">
    <div class="indent">
      <xsl:apply-templates/>
    </div>
  </xsl:template>

<!-- An unordered and an ordered list

     @begin{itemize}
       @begin{item}
         Text of the first item.
       @end{item}
       ...
     @end{itemize}

     @begin{enumerate}
       @begin{item}
         Text of the first item.
       @end{item}
       ...
     @end{enumerate}
 -->

  <xsl:template match="itemize">
    <ul class="disc">
      <xsl:apply-templates/>
    </ul>
  </xsl:template>

  <xsl:template match="enumerate">
    <ol>
      <xsl:apply-templates/>
    </ol>
  </xsl:template>

  <xsl:template match="item">
    <li>
      <xsl:apply-templates/>
    </li>
  </xsl:template>

<!-- A definition list

     @begin[attr]{table}
       @begin[first entry]
         Text of the first entry.
       @end{entry}
       ...
     @end{table}

     attr: a font for the definition like code, sym, arg, var, ...
-->

  <xsl:template match="table">
    <dl>
      <xsl:apply-templates>
        <xsl:with-param name="attribute" select="@table"/>
      </xsl:apply-templates>
    </dl>
  </xsl:template>

  <xsl:template match="entry">
    <xsl:param name="attribute"/>
    <div class="table-entry">
    <xsl:choose>
      <xsl:when test="$attribute">
        <dt>

            <span class="{$attribute}">
              <xsl:value-of select="@entry"/>
            </span>

        </dt>
      </xsl:when>
      <xsl:otherwise>
          <dt>
            <i><xsl:value-of select="@entry"/></i>
          </dt>
      </xsl:otherwise>
    </xsl:choose>
    <dd>
      <xsl:apply-templates/>
    </dd>
    </div>
  </xsl:template>

  <xsl:template match="simple-table">
    <div class="simple-table">
    <dl>
      <xsl:apply-templates>
        <xsl:with-param name="attribute" select="@simple-table"/>
      </xsl:apply-templates>
    </dl>
    </div>
  </xsl:template>

<!-- Cross references to Lisp symbols -->

  <xsl:template match="link">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <!-- Match a cross reference within the documentation -->
  <xsl:template match="ref">
    <a href="#{@id}">
        <xsl:apply-templates/>
    </a>
  </xsl:template>

<!-- Replace with the short name sym -->
  <xsl:template match="symbol">
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
  </xsl:template>

  <xsl:template match="sym">
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
  </xsl:template>

  <xsl:template match="val">
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

  <xsl:template match="var">
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
  </xsl:template>

  <xsl:template match="fun">
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
  </xsl:template>

  <xsl:template match="setf">
    <xsl:choose>
      <xsl:when test="@id">
        <span class="sym">
          <xsl:text>(setf </xsl:text>
          <a href="#{@id}">
            <xsl:apply-templates/>
          </a>
          <xsl:text>)</xsl:text>
        </span>
      </xsl:when>
      <xsl:otherwise>
        <span class="sym">
          <xsl:text>(setf </xsl:text>
          <xsl:apply-templates/>
          <xsl:text>)</xsl:text>
        </span>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="gen">
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
  </xsl:template>

  <xsl:template match="operator">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match="macro">
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
  </xsl:template>

  <xsl:template match="type">
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
  </xsl:template>

  <xsl:template match="class">
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
  </xsl:template>

  <xsl:template match="struct">
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
  </xsl:template>

  <xsl:template match="condition">
    <a href="#{@id}">
      <span class="sym">
        <xsl:apply-templates/>
      </span>
    </a>
  </xsl:template>

  <xsl:template match ="image">
    <span class="image">
      <img src="../figures/{@image}.png"/>
      <br/>
      <xsl:apply-templates/>
    </span>
  </xsl:template>

</xsl:stylesheet>
