#' Sort photos by date
#'
#' For each photo file, you can move it, copy it, skip it, or delete it.
#'
#' @param from character. Path to directory that contains photo files.
#'
#' @param to character. Path to destination directory to move photo files. Will
#'   be created automatically if it doesn't exist.
#'
#' @param subdir character (default: \code{NULL}). Optional format to create subdirectory
#'   based on date photo was first created. Use the POSIX formatting described
#'   in \code{\link{strptime}}. For example, \code{"\%Y \%m \%B"} is the 4-digit
#'   year, 2-digit month, and full month name.
#'
#' @return Invisibly returns path to destination directory (input argument
#'   \code{to}) as an \code{\link[fs]{fs_path}} object.
#'
#' @examples
#' \dontrun{
#'
#' sort_photos("~/Dropbox/Camera Uploads/", "~/Dropbox/family-photos/", subdir = "%Y %m %B")
#' }
#'
#' @export
sort_photos <- function(from, to, subdir = NULL) {
  from <- fs::path(from)
  to <- fs::path(to)

  stopifnot(fs::dir_exists(from))
  if (!fs::dir_exists(to)) {
    fs::dir_create(to)
    cli::cat_bullet(glue::glue("Created directory {to}"))
  }

  photos <- fs::dir_ls(path = from)

  cli::cat_bullet(glue::glue("Found {length(photos)} files in {from}"))

  for (photo in photos) {
    plot_photo(photo)
    photo_name <- fs::path_file(photo)
    photo_date <- as.Date(fs::file_info(photo)$modification_time)
    cli::cat_print(glue::glue("{photo_name} created on {photo_date}"))
    destination <- to
    if (!is.null(subdir)) {
      destination <- fs::path(destination, format(photo_date, subdir))
      fs::dir_create(destination)
    }
    cli::cat_print(glue::glue("Destination directory: {destination}"))
    decision <- utils::menu(choices = c("Move", "Copy", "Skip", "Delete", "Exit"))
    action <- switch(decision, "move", "copy", "skip", "delete")
    if (is.null(action)) {
      cli::cat_line("Exiting...")
      return(invisible(to))
    }
    if (action == "move") {
      cli::cat_line("Moving file")
      fs::file_move(photo, fs::path(destination, photo_name))
    } else if (action == "copy") {
      cli::cat_line("Copying file")
      fs::file_copy(photo, fs::path(destination, photo_name))
    } else if (action == "skip") {
      cli::cat_line("Skipping file")
      next()
    } else if (action == "delete") {
      cli::cat_line("Deleting file")
      fs::file_move(photo, fs::path_temp())
    }
  }

  return(invisible(to))
}

plot_photo <- function(path) {
  ext <- fs::path_ext(path)
  ext <- tolower(ext)
  if (ext == "png") {
    plot_png(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    plot_jpg(path)
  } else {
    warning(glue::glue("Don't know how to read file extension {ext}"),
            call. = FALSE, immediate. = TRUE)
  }
}

# source: https://stackoverflow.com/a/23861028/2483477
plot_jpg <- function(path) {
  photo <- jpeg::readJPEG(path, native = TRUE)
  graphics::plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
  graphics::rasterImage(photo, 0, 0, 1, 1)
}


# source: https://stackoverflow.com/a/23861028/2483477
plot_png <- function(path) {
  photo <- png::readPNG(path, native = TRUE)
  graphics::plot.new()
  graphics::rasterImage(photo, 0, 0, 1, 1)
}
