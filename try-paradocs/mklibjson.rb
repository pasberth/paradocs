require "json"

pairs = `git ls-files ../lib`.lines.flat_map do |lib|
  lib.chomp!
  key = lib.sub(/^\.\.\/lib\//, "")

  [key, File.read(lib)]
end

puts JSON.dump(Hash[*pairs])