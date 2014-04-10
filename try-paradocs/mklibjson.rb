require "json"
require "pathname"

dir = File.dirname(__FILE__)
libdir = Pathname.new("#{dir}/../lib").cleanpath.to_s

pairs = `git ls-files #{libdir}`.lines.flat_map do |lib|
  lib.chomp!
  lib = Pathname.new(lib).cleanpath.to_s
  key = lib.sub(/^#{Regexp.escape libdir}\//, "")

  [key, File.read(lib)]
end

puts JSON.dump(Hash[*pairs])