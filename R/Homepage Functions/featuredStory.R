featuredStory <- function(
  myURL,
  myImage,
  myTitle,
  myDescription
) {
  HTML(
    paste0(
      "<div class = 'featuredStory'>
        <a href='", myURL, "'>
          <span class='link-spanner'></span>
        </a>
        <img src='Featured Stories/", myImage, "'>
        <h1>", myTitle, "</h1>
        <p>", myDescription, "</p>
      </div>"
    )
  )
}



