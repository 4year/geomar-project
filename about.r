function(){
	tabPanel("About",
		HTML('<div style="float: right;margin: 0 5px 5px 10px;"><iframe width="560" height="315" src="https://www.youtube.com" frameborder="0" allowfullscreen></iframe></div>'),
		p(style="text-align:justify;color:white",'This web application shows the variation of different elements in the Ocean. We have a list of observation of elements that are measured over 
		a time of 552 hours. We can plot based on the '),
		p(style="text-align:justify;color:white",strong('Suggestions:'),'You may download a graphic in pdf form for your convenience using the download button.
		Any plot you produce on the Conditional Plots main tab, based on your selection of choise and variables, is downloadable in pdf form.
		Whenever you click the download button, you get whatever graphic is currently displayed in your browser.
		Formatting of the pdf plot will not match the browser plot exactly, but it will be a close approximation.'),
		p(style="text-align:justify;color:white",'Several other informations are provided below for your reference.'),
		strong('Download source data'),
		br(),
		a('Link will be provided to github repository', href="http://www.google.com", target="_blank"),
		br(),
		p(),

		HTML('<div style="clear: left;"><img src="" alt="" style="float: left;color:white; margin-right:5px" /></div>'),
		strong('Author'),
	
		br(),
		
		div(class="row-fluid",
			div(class="span4",strong('Related apps'),
				p(HTML('<ul>'),
				  HTML('<li>'),a("Arctic Sea Ice Extents and Concentrations", href="http://shiny.snap.uaf.edu/sea_ice_coverage/", target="_blank"),HTML('</li>'),
				HTML('</ul>')),
				strong('Code'),
				p('Source code available at',
				a('GitHub', href="https://github.com/", target="_blank")),
				br()
			),
			div(class="span4",
				strong('References'),
				p(HTML('<ul>'),
					HTML('<li>'),a('Coded in R', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
					HTML('<li>'),a('Built with the Shiny package', href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
					HTML('<li>'),"Primary supporting R packages",HTML('</li>'),
					HTML('<ul>'),
						HTML('<li>'),a('hexbin', href="http://cran.r-project.org/web/packages/hexbin/", target="_blank"),HTML('</li>'),
						HTML('<li>'),a('reshape2', href="http://cran.r-project.org/web/packages/reshape2/index.html", target="_blank"),HTML('</li>'),
						HTML('<li>'),a('shiny', href="http://cran.r-project.org/web/packages/shiny/index.html", target="_blank"),HTML('</li>'),
						HTML('<li>'),a('gridextra', href="http://cran.r-project.org/web/packages/gridextra/index.html", target="_blank"),HTML('</li>'),
					HTML('<li>'),a('ggplot2', href="http://cran.r-project.org/web/packages/ggplot2/index.html", target="_blank"),HTML('</li>'),
					HTML('<ul>'),
				HTML('</ul>'))
			)
		),
		value="about"
	)
}
