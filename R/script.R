

pkg <- pkgdown:::section_init(".", depth = 1L)


data <- pkgdown:::data_reference_index(pkg)

template <- '
<div class="row">
  <main id="main" class="col-md-9">
    <div class="page-header">
      {{#logo}}<img src="{{logo.src}}" class="logo" alt="" >{{/logo}}
      <h1>{{{pagetitle}}}</h1>
    </div>

    {{#rows}}<div id="{{slug}}" class="section level2">
      {{#title}}<h2>{{{.}}}</h2>{{/title}}
      {{#subtitle}}<h3>{{{.}}}</h3>{{/subtitle}}
      {{#desc}}<p class="section-desc">{{{desc}}}</p>{{/desc}}

      {{#topics}}<dl>
        <dt>
          {{#has_icons}}{{#icon}}<a class="icon" href="{{path}}"><img src="icons/{{{.}}}" alt=""/></a>{{/icon}}{{/has_icons}}
          {{#aliases}}<code><a href="{{path}}">{{{.}}}</a></code> {{/aliases}}
        </dt>
        <dd>{{{title}}}</dd>
      </dl>{{/topics}}
    </div>{{/rows}}
  </main>

  <aside class="col-md-3">
    <nav id="toc">
      <h2>{{#translate}}{{on_this_page}}{{/translate}}</h2>
    </nav>
  </aside>
</div>
'


template <- '
{{#logo}}
![]({{logo.src}})
{{/logo}}
# {{{pagetitle}}}

{{#rows}}
## {{{title}}}

### {{{subtitle}}}

{{{desc}}}

{{#topics}}
- **Link:**
    {{#has_icons}}{{#icon}}![](icons/{{{.}}}) [Link]({{path}}) {{/icon}}{{/has_icons}}
    {{#aliases}} [{{{.}}}]({{path}}) {{/aliases}}

- **Description:** {{{title}}}
{{/topics}}
{{/rows}}
'

cat(whisker::whisker.render(template, data))
