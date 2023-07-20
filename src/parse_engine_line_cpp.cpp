#include <Rcpp.h>
#include <string>
#include <vector>
#include <sstream>

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector parse_engine_line_cpp(std::string engine_line, CharacterVector tag_names) {
  // Split the input string by space and store the tokens in a vector
  std::vector<std::string> split_engine_line;
  std::stringstream ss(engine_line);
  std::string token;
  while (getline(ss, token, ' ')) {
    split_engine_line.push_back(token);
  }

  // Initialize the output character vector with the same size as tag_names
  CharacterVector tag_values(tag_names.size(), NA_STRING);

  // Loop through each tag name in tag_names
  for (int i = 0; i < tag_names.size(); i++) {
    std::string tag_name = as<std::string>(tag_names[i]);
    // Search for the tag name in split_engine_line
    auto it = find(split_engine_line.begin(), split_engine_line.end(), tag_name);
    if (it != split_engine_line.end()) {
      // If the tag name is found, determine its value based on its position and the values of the following tokens
      size_t pos = distance(split_engine_line.begin(), it);
      if (tag_name == "score") {
        // If the tag is 'score', return the value and type of score
        if (pos + 2 < split_engine_line.size()) {
          tag_values[i] = split_engine_line[pos + 1] + " " + split_engine_line[pos + 2];
        }
      } else if (tag_name == "string") {
        // If tag is 'string', return the value of the string to the end of line
        std::string value;
        for (size_t j = pos + 1; j < split_engine_line.size(); j++) {
          value += split_engine_line[j] + " ";
        }
        tag_values[i] = value.substr(0, value.size() - 1);
      } else if (pos + 1 < split_engine_line.size() && tag_name != "currline") {
        // Next deal with numeric tags
        std::string next_token = split_engine_line[pos + 1];
        // Be careful: currline can look like a numeric value.
        // If value is not a UCI move and tag is not 'currline', return the value
        if (next_token.find_first_not_of("0123456789") == std::string::npos) {
          tag_values[i] = next_token;
        }
      }

      std::string value;
      bool has_number = false;
      for (size_t j = pos + 1; j < split_engine_line.size(); j++) {
        std::string move = split_engine_line[j];
        if (move.size() >= 4 && move.size() <= 5 &&
            move[0] >= 'a' && move[0] <= 'h' &&
            move[1] >= '1' && move[1] <= '8' &&
            move[2] >= 'a' && move[2] <= 'h' &&
            move[3] >= '1' && move[3] <= '8' &&
            (move.size() == 4 ||
            (move[4] == 'q' || move[4] == 'r' || move[4] == 'b' || move[4] == 'n'))) {
          // Next deal with move tags
          value += move + " ";
        } else if (!has_number && move.find_first_not_of("0123456789") == std::string::npos) {
          value += move + " ";
          has_number = true;
        } else {
          break;
        }
      }
      if (!value.empty()) {
        // Return the series of moves
        tag_values[i] = value.substr(0, value.size() - 1);
      }
    } else {
      // If the tag is not present, return NA
      tag_values[i] = NA_STRING;
    }
  }

  return tag_values;
}
