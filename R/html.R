is_img_line = function(x) grepl('^<img src=".* alt="', x)

# remove HTML tags and remove extra spaces
strip_html = function(x) {
  x = gsub('<!--.*?-->', '', x)  # remove comments
  x = gsub('<[^>]+>', '', x)
  x = gsub('\\s{2,}', ' ', x)
  x
}