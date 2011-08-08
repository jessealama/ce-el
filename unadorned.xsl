<?xml version='1.0' encoding='UTF-8'?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>
  <xsl:param name="emit-html">
    <xsl:text>0</xsl:text>
  </xsl:param>

  <xsl:template match="*">
    <xsl:choose>
      <xsl:when test="not($emit-html = &quot;0&quot;)">
        <xsl:variable name="element-name" select="local-name ()"/>
        <xsl:element name="{$element-name}">
          <xsl:for-each select="@*">
            <xsl:copy-of select="."/>
          </xsl:for-each>
          <xsl:if test="$element-name = &quot;head&quot;">
            <xsl:element name="style">
              <xsl:attribute name="type">
                <xsl:text>text/css</xsl:text>
              </xsl:attribute>
              <xsl:text>p.elicited-from-unadorned-stylesheet { border: thin; background-color: silver; }</xsl:text>
            </xsl:element>
          </xsl:if>
          <xsl:apply-templates select="child::node()"/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="child::node()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text()[not(normalize-space() = &quot;&quot;) and preceding-sibling::*]">
    <xsl:choose>
      <xsl:when test="$emit-html = &quot;0&quot;">
        <xsl:variable name="preceding-sibling-element-name" select="local-name (preceding-sibling::*[1])"/>
        <xsl:variable name="message" select="concat (&quot;Warning: unadordned whitespace detected after an element named &quot;, $preceding-sibling-element-name)"/>
        <xsl:message>
          <xsl:value-of select="$message"/>
        </xsl:message>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="p">
          <xsl:attribute name="class">
            <xsl:text>elicited-from-unadorned-stylesheet</xsl:text>
          </xsl:attribute>
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:if test="not($emit-html = &quot;0&quot;)">
      <xsl:value-of select="."/>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
