<!DOCTYPE html>
	<!--
	Google HTML5 slide template

Authors: Luke Mah?? (code)
Marcin Wichary (code and design)

Dominic Mazzoni (browser compatibility)
Charles Chen (ChromeVox support)

URL: http://code.google.com/p/html5slides/
	-->
	
<html>
<head>
	
	<meta charset='utf-8'>
	<title>{{ title }}</title>
  <meta name="description" content="{{title}}">
  <meta name="author" content="{{author}}">
  <meta name="generator" content="slidify" />
	
	<!-- LOAD STYLE SHEETS -->
	<link rel="stylesheet" href="{{lib_path}}/html5slides/{{theme}}/styles.css">
	<link rel="stylesheet" href="{{lib_path}}/html5slides/{{theme}}/uulm.css">
	<link rel="stylesheet" href="{{lib_path}}/{{highlighter}}/styles/{{histyle}}.css">
  {{> user_css}}	
  
</head>
<body style='display: none'>
	<section class='slides layout-{{layout}} template-{{template}}'>
    {{#slides}}
	  <article class = "{{classes}}" id = "{{id}}"> 
	    {{{ slide }}}
    </article>
    {{/slides}}  
  </section>
</body>
  <!-- LOAD JAVASCRIPTS  -->
	<script src='{{lib_path}}/html5slides/{{theme}}/slides.js'></script>
	{{> mathjax}}
	{{> highlight_js}}
	{{> google_prettify}}
	{{> user_js}}
	
</html>

