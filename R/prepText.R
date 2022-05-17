#' prepareText
#'
#' This function returns a numeric sequence corresponding to a text
#' file, suitable for RQA use.
#' @param <x> text file name
#' @keywords clean text
#' @export
#' @examples
#' prepareText()
#'
prepareText <- function(x) {          # Text file to read
  # ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  #                                                               #
  # This is a little helper function to clean up the text         #
  #  from the lyrics and return a series of numbers.              #
  #                                                               #
  # You do NOT need to understand any of these details.           #
  #  Original function courtesy of Aaron Likens                   #
  #  Modified somewhat by BPR                                     #
  #                                                               #
  # ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  require(readtext)                   # readtext()
  require(tm)                         # Text manipulation functions

  if(file.exists(x)) {                # Check for file existence
    readtext(x)$text -> textIn        # Read the text
  } else {
    cat("Input file does not exist!\n")
    return(NULL)
  }

  if(is.character(textIn) == FALSE) { # Validate textIn
    cat("Data are not textual!\n")
    return(NULL)
  }

  gsub('\\n',' ', textIn) ->          # Get rid of the line breaks
    rawText
  gsub('\\r',' ', textIn) ->          # Get rid of any hard returns
    rawText
  Corpus(VectorSource(rawText)) ->    # Create a dictionary from the text.
    ts
  tm_map(ts, removePunctuation) ->    # Remove the punctuation
    ts
  tm_map(ts, removeNumbers) ->        # Remove numbers
    ts
  tm_map(ts, tolower) ->              # Make everything lower case
    ts
  tm_map(ts, stripWhitespace) ->      # Get rid of white spaces
    ts
  tm_map(ts, stemDocument) ->         # Reduce words to their stems
    ts
  tm_map(ts, PlainTextDocument) ->    # Make it all plain text
    ts
  as.character(ts[[1]]) ->            # Make words into string of numbers
    ts
  unlist(strsplit(ts, ' ')) ->        # Break each number into a separate
    words                             #  entry.
  unique(words) ->                    # Get a list of unique words
    unique_words

  # Derive a an ordered sequence that characterizes words in terms of their
  #  order of appearance. Duplicates retain the same number:
  as.vector(                          # Need a vector, not a list
    sapply(words,                     # Apply to every word
           function(x) {
             which(x == unique_words) # Which unique word is x?
           }
    )) -> word_series                 # Make these into a series
  return(word_series)
}

