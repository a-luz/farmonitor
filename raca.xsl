<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">
    <html>
      <body>
	<h2>Relatorio Raça:
	<xsl:value-of select="relatorio/raca"/>
	</h2>
	<table border="1">
	  <tr bgcolor="#9acd32">
	    <th style="text-align:left">Codigo</th>
	    <th style="text-align:left">Sexo</th>
	    <th style="text-align:left">Cor</th>
	  </tr>
	  <xsl:for-each select="relatorio/animal">
	    <tr>
	      <td>
		<xsl:value-of select="codigo"/>
	      </td>
	      <td>
		<xsl:value-of select="sexo"/>
	      </td>
	      <td>
		<xsl:value-of select="cor"/>
	      </td>	
	    </tr>		
	  </xsl:for-each>
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
