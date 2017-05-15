<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">
    <html>
      <body>
	<h2>Relatorio dia:
	<xsl:value-of select="relatorio/dia"/>
	</h2>
	<table border="1">
	  <tr bgcolor="#9acd32">
	    <th style="text-align:left">Codigo Animal</th>
	    <th style="text-align:left">Quantidade</th>
	  </tr>
	  <xsl:for-each select="relatorio/producao">
	    <tr>
	      <td>
		<xsl:value-of select="codigo"/>
	      </td>
	      <td>
		<xsl:value-of select="quantidade"/>
	      </td>
	    </tr>		
	  </xsl:for-each>
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
