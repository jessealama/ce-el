<?xml version='1.0' encoding='UTF-8'?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" indent="yes"/>

  <xsl:template match="*">
    <xsl:variable name="element-name" select="local-name ()"/>
    <xsl:element name="{$element-name}">
      <xsl:for-each select="@*">
        <xsl:copy-of select="."/>
      </xsl:for-each>
      <xsl:apply-templates select="child::node()"/>
    </xsl:element>
  </xsl:template>

  <xsl:template match="text()[not(normalize-space() = &quot;&quot;) and preceding-sibling::*]">
    <xsl:element name="p">
      <xsl:attribute name="class">
        <xsl:text>elicited-from-unadorned-stylesheet</xsl:text>
      </xsl:attribute>
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>
</xsl:stylesheet>
