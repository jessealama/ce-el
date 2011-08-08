<?xml version='1.0' encoding='UTF-8'?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>

  <xsl:template match="node()">
    <xsl:variable name="element-name" select="local-name ()"/>
    <xsl:element name="{$element-name}">
      <xsl:for-each select="@*">
        <xsl:copy-of select="."/>
      </xsl:for-each>
      <xsl:apply-templates select="child::text() | child::node()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:variable name="trimmed" select="normalize-space ()"/>
    <xsl:choose>
      <xsl:when test="$trimmed = &quot;&quot;">
        <xsl:value-of select="."/>
      </xsl:when>
      <xsl:when test="preceding-sibling::*">
        <xsl:element name="p">
          <xsl:attribute name="class">
            <xsl:text>elicited-from-unadorned-stylesheet</xsl:text>
          </xsl:attribute>
          <xsl:value-of select="."/>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="."/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
