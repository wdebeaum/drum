Skip = Object.new
def Skip.===(e)
  Integer === e and e < 0
end

class Array
  # Index into an array with contiguous sequences of 0s replaced by the negated
  # length of the sequence. e.g. logical [1,2,0,0,0,3,4,5,6] corresponds to
  # physical [1,2,-3,3,4,5,6]. The array must contain no other negative
  # integers. You may provide a different element to return when a skip is
  # indexed.
  def index_with_skips(logical_index, skipped_element=0)
    return reverse.index_with_skips(-1-logical_index, skipped_element) if (logical_index < 0)
    physical_index = 0
    until (logical_index <= 0)
      if (Skip === self[physical_index])
	logical_index += self[physical_index]
      else
	logical_index -= 1
      end
      physical_index += 1
    end
    return ((logical_index < 0 or Skip === self[physical_index]) ? skipped_element : self[physical_index])
  end

  # Like each_with_index, but with skips (see index_with_skips). During skips,
  # skipped_element is yielded.
  def each_with_index_with_skips(skipped_element=0)
    return to_enum(__method__, skipped_element) unless (block_given?)
    i = 0
    each { |e|
      if (Skip === e)
	(-e).times { |j|
	  yield skipped_element, i
	  i += 1
	}
      else
	yield e, i
	i += 1
      end
    }
  end

  # Replace contiguous sequences of skippable elements with their negated
  # lengths.
  # ary.to_array_with_skips # skips 0s
  # ary.to_array_with_skips(elt) # skips everything elt ==
  # ary.to_array_with_skips &block # skips everything the block returns true for
  def to_array_with_skips(skippable_element=0)
    element_skippable =
      if (block_given?)
	lambda { |e| yield e }
      else
	lambda { |e| skippable_element == e }
      end
    ret = []
    current_skip = 0
    each { |e|
      if (element_skippable[e])
	current_skip -= 1
      elsif (Skip === e)
	raise "Can't add skips to array that already has them!"
      else
        unless (current_skip == 0)
	  ret << current_skip
	  current_skip = 0
	end
        ret << e
      end
    }
    ret << current_skip if (current_skip < 0)
    return ret
  end
end

