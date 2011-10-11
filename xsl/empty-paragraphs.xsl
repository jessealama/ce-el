<?xml version='1.0' encoding='UTF-8'?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>

  <!-- This stylesheet removes empty paragraphs. -->
  <xsl:template match="*">
    <xsl:variable name="element-name" select="local-name ()"/>
    <xsl:element name="{$element-name}">
      <xsl:for-each select="@*">
        <xsl:copy-of select="."/>
      </xsl:for-each>
      <xsl:apply-templates select="child::node()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="p[normalize-space() = &quot;&quot;]"/>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>
</xsl:stylesheet>
