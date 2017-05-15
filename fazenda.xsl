<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template match="/">
    <html>
      <body>
	<h2>Relatorio: 
	  <xsl:value-of select="relatorio/animal"/>
	</h2>
	<table border="1">
	  <tr bgcolor="#9acd32">
	    <th style="text-align:left">Dia</th>
	    <th style="text-align:left">Produçao</th>
	    <th style="text-align:left">Diagnostico</th>
	    <th style="text-align:left">Tratamento</th>
	  </tr>
	  <xsl:for-each select="relatorio/dia">
	    <tr>
	      <td>
		<xsl:value-of select="data"/>
	      </td>
	      <td>
		 <xsl:for-each select="qtd">
		  <table border="0">
		    <td>
		      <xsl:value-of select="."/>
		    </td>
		  </table>
		</xsl:for-each>
	       </td>
	       <td>
		  <xsl:for-each select="diag/doenca">
		  <table border="0">
		    <td>
		      <xsl:value-of select="."/>
		    </td>
		  </table>
		</xsl:for-each>
		</td>
		<td>
		  <xsl:for-each select="diag/tratamento">
		  <table border="0">
		    <td>
		      <xsl:value-of select="."/>
		    </td>
		  </table>
		</xsl:for-each>
		</td>
	    </tr>
	  </xsl:for-each>
	</table>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
