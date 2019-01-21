const markdownIt = require("markdown-it");
const mk = require('markdown-it-katex');
const prism = require('markdown-it-prism');
const pluginRss = require("@11ty/eleventy-plugin-rss");

module.exports = (function(eleventyConfig) {
  eleventyConfig.addPlugin(pluginRss);
  eleventyConfig.addPassthroughCopy("src/img/");
  eleventyConfig.addPassthroughCopy("src/style/");
  eleventyConfig.addPassthroughCopy("src/style/themes");
  eleventyConfig.addCollection("allPosts", function(collection) {

    return collection.getAllSorted().filter(function(v) {
      if (v.data.tags) {
        return v.data.tags.includes("post") ||
          v.data.tags.includes("TIL")
      }
      return false;
    }).reverse()
  })

  eleventyConfig.addCollection("allTags", function(collection) {

    var tags = collection.getAllSorted().map(function(v) {
      return v.data.tags || []
    })
    return [].concat(...tags)
      .filter((v, index, self) => self.indexOf(v) == index)
      .sort((a, b) =>
        a.toLowerCase().localeCompare(b.toLowerCase())
      )
  });

  // Markdown
  // Katex for Latex
  let options = {
    html: true,
    breaks: true,
    linkify: true
  };
  let md = markdownIt(options);
  eleventyConfig.setLibrary("md", md);
  md.use(mk)
  md.use(prism)


  return {
    passthroughFileCopy: true,
    dir: {
      input: "src",
      output: "_site"
    }
  }
});
