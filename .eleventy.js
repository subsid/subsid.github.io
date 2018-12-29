const markdownIt = require("markdown-it");
const mk = require('markdown-it-katex');
const prism = require('markdown-it-prism');

module.exports = (function(eleventyConfig) {
  eleventyConfig.addFilter("filesize", function(path) {
    return "0 KB";
  })

  eleventyConfig.addPassthroughCopy("src/img/");
  eleventyConfig.addPassthroughCopy("src/style/");
  eleventyConfig.addPassthroughCopy("src/style/themes");

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
    passthroughFileCopy: true
  }
});
