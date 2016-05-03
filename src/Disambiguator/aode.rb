require_relative 'skips'

$debug = false

# Averaged One-Dependence Estimators classifier
#
# Based on the paper:
#  Geoffrey I. Webb, et al.
#  "Not So Naive Bayes: Aggregating One-Dependence Estimators"
#
# Code by William de Beaumont <wbeaumont@ihmc.us>
#
# Training just accumulates frequency counts F(y,x_i,x_j), where:
#
# - y is the class
# - i,j index features
# - x_i,x_j are values of those features
#
# To classify, we compute for each class y for a given feature list X:
#
#   Phat(y | X) \propto \sum_i Phat(y,x_i) \prod_j Phat(x_j | y,x_i)
#
# (read Phat as \hat{P}, probability estimate)
# and then normalize across all classes to get Phat(y | X). The idea is that
# this is like a Naive Bayes classifier, but with a weakened
# feature-independence assumption, where we consider all features (j) to depend
# on one other feature at a time (i), and average the results. The component
# probability estimates are computed directly from the frequency counts, with a
# Laplace estimator to cover cases unseen in training:
#
#   Phat(y,x_i) =
#     (F(y,x_i) + 1) /
#     (K + kv_i)
#
#   Phat(x_j | y,x_i) =
#     (F(y,x_i,x_j) + 1) /
#     (F(y,x_i) + v_j)
#
# where:
#
# - F(y,x_i) = F(y,x_i,x_i)
# - K is the number of training instances
# - k is the number of classes
# - v_i,v_j are the number of values for feature i,j respectively
#   (except for the :NIL feature value, which is always included even if it's
#   not seen in training)
#
# A number of cases are skipped in the above sum/product:
#
# - When i == j, because the actual probability P(x_i | y,x_i) == 1.
# - When v_i or v_j <= 1 (this time including :NIL), because that means that
#   feature doesn't contribute anything.
# - When x_i or x_j was not seen in training individually, because then we
#   don't know how they contribute.
#
# In the code (but not the math above), the class is generally treated as x_0,
# so that:
#
#   F(y) = F(y,x_0=y)
#
# And we can use that to imitate a true Naive Bayes classifier, or even just
# pick the most frequent class overall every time.
#
class AODE
  attr_reader(
    # Total number of instances in the training data. (K)
    :num_instances,
    # List of lists of feature values (Symbols), one list per feature, first
    # feature being the class.
    :feature_value_lists,
    # Ideally @counts would be a 5D matrix of instance counts, the dimensions
    # being:
    #
    # - the class y
    # - the index of one feature i
    # - the value of that feature x_i
    # - the index of another feature j
    # - the value of that feature x_j
    #
    # but many of the entries would be 0 (because some combinations would never
    # be seen, and because features have different numbers of values), and half
    # would be redundant since i and j are symmetrical.
    #
    # Instead, we store arrays with skips (see skips.rb) nested 5 deep, and
    # assume we have only the upper triangle of the symmetric i,j plane, so
    # that i <= j.
    #
    # In the notional 5D matrix, since y is treated as the first feature,
    #   F(y) = @counts[index of y][0][index of y][0][index of y]
    # Also,
    #   F(y,x_i) = @counts[index of y][i][index of x_i][i][index of x_i]
    # And of course,
    #   F(y,x_i,x_j) = @counts[index of y][i][index of x_i][j][index of x_j]
    :counts
  )

  def initialize
    @num_instances = 0
    @feature_value_lists = []
    @counts = []
  end

  # Given a value (Symbol) for each feature (except the first feature, which is
  # the class), return a Hash mapping classes (Symbols) to probabilities
  # (Floats in 0.0..1.0).
  def distribution_for_instance(feature_values)
    raise "Expected #{@feature_value_lists.size-1} features, but got #{feature_values.size} features: #{feature_values.inspect}" unless (feature_values.size == @feature_value_lists.size-1)
    feature_value_indices =
      [nil] + # make room for class
      feature_values.each_with_index.collect { |v, i|
        @feature_value_lists[i+1].index(v)
      }
    $stderr.puts "feature_values=#{feature_values.inspect}\nfeature_value_indices=#{feature_value_indices.inspect}" if ($debug)
    ret = {}
    # k
    num_classes = @feature_value_lists[0].size
    $stderr.puts "K=#{@num_instances}, k=#{num_classes}" if ($debug)
    @counts.each_with_index_with_skips([-@feature_value_lists.size]) { |class_counts, class_index|
#     Notionally:
#      feature_value_indices[0] = class_index
      # NB: don't need F(y)/Phat(y), in the paper it's just for Naive Bayes.
      sum = 0
      class_counts.each_with_index_with_skips { |i_counts, i|
        next if ( i == 0 or # skip class
		  feature_value_indices[i].nil? # skip unknown values
		)
	# v_i
	i_num_values = @feature_value_lists[i].size
	next if (i_num_values <= 1) # skip trivial features
	x_i_counts = nil
	class_x_i_count = 0
	if (i_counts == 0) # this part of the @counts table skipped
	  # we didn't see this feature value in this situation in training,
	  # all further counts are 0
	  x_i_counts = [-(@feature_value_lists.size - i)]
	else
          x_i_counts = i_counts.index_with_skips(feature_value_indices[i], [-(@feature_value_lists.size - i)])
	  # F(y,x_i)
	  class_x_i_count =
	    x_i_counts.
	    index_with_skips(0,[-i_num_values]).
	    index_with_skips(feature_value_indices[i])
	end
	$stderr.puts "F(y=#{@feature_value_lists[0][class_index]}, x_#{i-1}=#{feature_values[i-1]}) = #{class_x_i_count}" if ($debug)
	# Phat(y,x_i) = (F(y,x_i) + 1) / (K + k * v_i)
	class_x_i_prob =
	  (class_x_i_count + 1.0) /
	  (@num_instances + num_classes * (i_num_values-1))
	$stderr.puts "Phat(\") = #{class_x_i_prob} (v_#{i-1}=#{i_num_values-1})" if ($debug)
	product = 1

=begin
        # stupid simple way
	1.upto(@feature_value_lists.size-1) { |j|
	  next if ( i == j or # skip same feature
	  	    feature_value_indices[j].nil? # skip unknown values
		  )
	  j_num_values = @feature_value_lists[j].size
	  next if (j_num_values <= 1) # skip trivial features
	  ii, jj = *[i,j].sort
	  class_x_i_x_j_count =
	    class_counts.
	      index_with_skips(ii, [-@feature_value_lists[ii].size]).
	      index_with_skips(feature_value_indices[ii], [-(@feature_value_lists.size - ii)]).
	      index_with_skips(jj-ii, [-@feature_value_lists[jj].size]).
	      index_with_skips(feature_value_indices[jj])
	  $stderr.puts "F(y=#{@feature_value_lists[0][class_index]}, x_#{i-1}=#{feature_values[i-1]}, x_#{j-1}=#{feature_values[j-1]}) = #{class_x_i_x_j_count}" if ($debug)
	  # Phat(x_j | y,x_i) = (F(y,x_i,x_j) + 1) / (F(y,x_i) + v_j)
	  x_j_prob_given_class_x_i =
	    (class_x_i_x_j_count + 1.0) /
	    (class_x_i_count + j_num_values - 1)
	  $stderr.puts "Phat(x_#{j-1}=#{feature_values[j-1]} | y=#{@feature_value_lists[0][class_index]},x_#{i-1}=#{feature_values[i-1]}) = #{x_j_prob_given_class_x_i} (v_#{j-1}=#{j_num_values-1})" if ($debug)
	  product *= x_j_prob_given_class_x_i 
	}
=end
	# count lower triangle
	$stderr.puts "lower triangle:" if ($debug)
	class_counts.each_with_index_with_skips { |j_counts, j|
	  next if ( j == 0 or # skip class
	            feature_value_indices[j].nil? # skip unknown values
		  )
	  break if (j >= i) # skip upper triangle
	  j_num_values = @feature_value_lists[j].size
	  next if (j_num_values <= 1) # skip trivial features
	  class_x_j_x_i_count = 0
	  unless (j_counts == 0)
	    # F(y,x_j,x_i)
	    class_x_j_x_i_count =
	      j_counts.
		index_with_skips(feature_value_indices[j], [-(@feature_value_lists.size - j)]).
		index_with_skips(i - j, [-i_num_values]).
		index_with_skips(feature_value_indices[i])
	  end
	  $stderr.puts "F(y=#{@feature_value_lists[0][class_index]}, x_#{i}=#{feature_values[i-1]}, x_#{j}=#{feature_values[j-1]}) = #{class_x_j_x_i_count}" if ($debug)
	  # Phat(x_j | y,x_i) = (F(y,x_i,x_j) + 1) / (F(y,x_i) + v_j)
	  x_j_prob_given_class_x_i =
	    (class_x_j_x_i_count + 1.0) /
	    (class_x_i_count + j_num_values - 1)
	  $stderr.puts "Phat(x_#{j}=#{feature_values[j-1]} | y=#{@feature_value_lists[0][class_index]},x_#{i}=#{feature_values[i-1]}) = #{x_j_prob_given_class_x_i} (v_#{j}=#{j_num_values-1})" if ($debug)
	  product *= x_j_prob_given_class_x_i
	}
	# count upper triangle
	$stderr.puts "upper triangle:" if ($debug)
	x_i_counts.each_with_index_with_skips { |j_counts, j_minus_i|
	  next if (j_minus_i == 0) # skip i==j case
	  j = j_minus_i + i
	  next if (feature_value_indices[j].nil?) # skip unknown values
	  # v_j
	  j_num_values = @feature_value_lists[j].size
	  next if (j_num_values <= 1) # skip trivial features
	  class_x_i_x_j_count = 0
	  unless (j_counts == 0)
	    # F(y,x_i,x_j)
	    class_x_i_x_j_count =
	      j_counts.
	      index_with_skips(feature_value_indices[j])
	  end
	  $stderr.puts "F(y=#{@feature_value_lists[0][class_index]}, x_#{i}=#{feature_values[i-1]}, x_#{j}=#{feature_values[j-1]}) = #{class_x_i_x_j_count}" if ($debug)
	  # Phat(x_j | y,x_i) = (F(y,x_i,x_j) + 1) / (F(y,x_i) + v_j)
	  x_j_prob_given_class_x_i =
	    (class_x_i_x_j_count + 1.0) /
	    (class_x_i_count + j_num_values - 1)
	  $stderr.puts "Phat(x_#{j}=#{feature_values[j-1]} | y=#{@feature_value_lists[0][class_index]},x_#{i}=#{feature_values[i-1]}) = #{x_j_prob_given_class_x_i} (v_#{j}=#{j_num_values-1})" if ($debug)
	  product *= x_j_prob_given_class_x_i 
	}

	$stderr.puts "product for y=#{@feature_value_lists[0][class_index]},i=#{i-1} is #{product}" if ($debug)
	sum += class_x_i_prob * product
      }
      $stderr.puts "sum for y=#{@feature_value_lists[0][class_index]} is #{sum}" if ($debug)
      ret[@feature_value_lists[0][class_index]] = sum
    }
    $stderr.puts "unnormalized ret=#{ret.inspect}" if ($debug)
    if (ret.values.all? { |v| v == 0 })
      return prior_distribution()
    else
      return normalize_distribution(ret)
    end
  end

  # Add the given instance ([class] + a value for each feature) to the counts
  # in the model. Call this for each training instance and then call #finish_training! 
  def train_instance(feature_values)
    # get indices for feature values, adding the features and values if
    # necessary
    feature_value_indices =
      feature_values.each_with_index.collect { |value, feature_index|
        if (feature_index >= @feature_value_lists.size)
          @feature_value_lists << (feature_index == 0 ? [] : [:NIL])
	end
        value_index = @feature_value_lists[feature_index].index(value)
	if (value_index.nil?)
	  @feature_value_lists[feature_index] << value
	  @feature_value_lists[feature_index].size - 1
	else
	  value_index
	end
      }
    @num_instances += 1
    @counts << [] if (feature_value_indices[0] >= @counts.size)
    class_counts = @counts[feature_value_indices[0]]
    feature_value_indices.each_with_index { |i_value_index, i|
      class_counts[i] ||= []
      i_counts = (class_counts[i][i_value_index] ||= [])
      (feature_value_indices.size - i).times { |j_minus_i|
        j_value_index = feature_value_indices[j_minus_i + i]
	i_counts[j_minus_i] ||= []
	i_counts[j_minus_i][j_value_index] ||= 0
        i_counts[j_minus_i][j_value_index] += 1
      }
    }
  end

  def finish_training!
#    delete_infrequent_feature_values(3) # m from the paper
    insert_skips()
  end

  # delete feature values we've seen fewer than min_freq times
  # FIXME when I use this, accuracy goes way down to around 15%
  def delete_infrequent_feature_values(min_freq)
    # find values that need to be deleted
    deleted_indices = [[]]*@feature_value_lists.size
    @feature_value_lists.each_with_index { |values, feature_index|
      next if (feature_index == 0) # skip class
      zeros = [[[0]*values.size]]
      values.each_with_index { |value, value_index|
        value_freq = 0
	@counts.each { |class_counts|
	  value_freq +=
	    ((class_counts[feature_index] || zeros
	     )[value_index] || zeros[0]
	     )[0][value_index]
	  break if (value_freq >= min_freq)
	}
	deleted_indices[feature_index] << value_index if (value_freq < min_freq)
      }
    }
    # delete them from @counts
    @counts.collect! { |class_counts|
      class_counts.each_with_index.collect { |i_counts, i|
        i_counts.each_with_index.collect_concat { |x_i_counts, i_value_index|
	  if (deleted_indices[i].include?(i_value_index))
	    []
	  elsif (x_i_counts.nil?)
	    [nil]
	  else
	    [x_i_counts.each_with_index.collect { |j_counts, j_minus_i|
	      j_counts.each_with_index.collect_concat { |x_j_count, j_value_index|
	        if (deleted_indices[j_minus_i + i].include?(j_value_index))
		  []
		else
		  [x_j_count]
		end
	      }
	    }]
	  end
	}
      }
    }
    # ...and from @feature_value_lists
    @feature_value_lists.each_with_index { |values, feature_index|
      @feature_value_lists[feature_index] =
        values.each_with_index.collect_concat { |value, value_index|
	  if (deleted_indices[feature_index].include?(value_index))
	    []
	  else
	    [value]
	  end
	}
    }
  end

  # Insert skips into @counts to save memory, and ensures the logical lists are
  # the right size.
  # Unfortunately this implementation requires enough space to store both the
  # uncompressed and the compressed versions. But the intent is to save memory
  # during classification, not necessarily during training.
  def insert_skips
    @counts.collect! { |class_counts|
      class_counts.each_with_index.collect { |i_counts, i|
        i_padding = [nil] * (@feature_value_lists[i].size - i_counts.size)
        (i_counts + i_padding).collect { |x_i_counts|
	  unless (x_i_counts.nil?)
	    x_i_counts.each_with_index.collect { |j_counts, j_minus_i|
	      j_padding = [nil] * (@feature_value_lists[j_minus_i + i].size - j_counts.size)
	      (j_counts + j_padding).to_array_with_skips { |e| e.nil? or e == 0 }
	    }.to_array_with_skips { |a| a.nil? or (a.size == 1 and Skip === a[0]) }
	  end
	}.to_array_with_skips { |a| a.nil? or (a.size == 1 and Skip === a[0]) }
      }.to_array_with_skips { |a| a.size == 1 and Skip === a[0] }
    }.to_array_with_skips { |a| a.size == 1 and Skip === a[0] }
  end

  
  # Like #distribution_for_instance, but drop the one-dependence stuff and
  # assume independent features.
  def naive_bayes_distribution_for_instance(feature_values)
    $stderr.puts "feature_values=#{feature_values.inspect}" if ($debug)
    feature_value_indices =
      [nil] + # make room for class
      feature_values.each_with_index.collect { |v, i|
        @feature_value_lists[i+1].index(v)
      }
    $stderr.puts "feature_value_indices=#{feature_value_indices.inspect}" if ($debug)
    ret = {}
    # k
    num_classes = @feature_value_lists[0].size
    $stderr.puts "num_classes=#{num_classes}; num_instances=#{@num_instances}" if ($debug)
    @counts.each_with_index_with_skips { |class_counts, class_index|
      feature_value_indices[0] = class_index
      # F(y)
      class_count =
        class_counts.
	index_with_skips(0, [-@feature_value_lists[0].size]).
	index_with_skips(class_index, [-@feature_value_lists.size]).
	index_with_skips(0, [-@feature_value_lists[0].size]).
	index_with_skips(class_index)
      # Phat(y) = (F(y) + 1) / (K + k)
      class_prob = (class_count + 1.0) / (@num_instances + num_classes)
      $stderr.puts "class_index=#{class_index}; class_count=#{class_count}; class_prob=#{class_prob}" if ($debug)
      product = class_prob
      class_counts.each_with_index_with_skips { |i_counts, i|
        next if (i == 0) # skip class
	# v_i
	i_num_values = @feature_value_lists[i].size
	x_i_counts = [-@feature_value_lists.size]
	class_x_i_count = 0
	unless (i_counts == 0 or feature_value_indices[i].nil?)
	  x_i_counts = i_counts.index_with_skips(feature_value_indices[i], [-(@feature_value_lists.size - i)])
	  # F(y,x_i)
	  class_x_i_count =
	    x_i_counts.
	    index_with_skips(0,[-i_num_values]).
	    index_with_skips(feature_value_indices[i])
	end
	# Phat(y,x_i) = (F(y,x_i) + 1) / (K + k * v_i)
	class_x_i_prob =
	  (class_x_i_count + 1) /
	  (@num_instances + num_classes * i_num_values)
	$stderr.puts "class_x_i_count=#{class_x_i_count}; i_num_values=#{i_num_values}; class_x_i_prob=#{class_x_i_prob}" if ($debug)
	product *= class_x_i_prob
      }
      $stderr.puts "product=#{product}" if ($debug)
      ret[@feature_value_lists[0][class_index]] = product
    }
    return normalize_distribution(ret)
  end

  # Like distribution_for_instance, but ignore the instance entirely and just
  # use the probability of each class globally.
  def prior_distribution
    ret = {}
    @counts.each_with_index_with_skips { |class_counts, class_index|
      # F(y)
      ret[@feature_value_lists[0][class_index]] = 1.0 * # prevent integer div.
        class_counts.
	index_with_skips(0, [-@feature_value_lists[0].size]).
	index_with_skips(class_index, [-@feature_value_lists.size]).
	index_with_skips(0, [-@feature_value_lists[0].size]).
	index_with_skips(class_index)
    }
    return normalize_distribution(ret)
  end
end

# Given a hash from class symbols to non-normalized probability estimates,
# return a version normalized by dividing each value by the sum of all the
# values.
def normalize_distribution(distribution)
  sum = distribution.values.reduce(:+)
  Hash[
    distribution.
    each_pair.
    collect { |class_symbol, prob| [class_symbol, prob / sum] }
  ]
end
