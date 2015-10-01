strExtractAll <- function(pattern, text, ...) {
	    mapply(function(x, y) {
		           if (is.na(x) || length(x) == 1 && x == -1) {
				               NA_character_
			           } else {
					               substring(y, x, x + attr(x, "match.length") - 1)
				           }
			       }, gregexpr(pattern, text, ...), text, SIMPLIFY = FALSE)
}

